module Main exposing (..)

import Data.Session exposing (Session)
import Html exposing (Html, form, map)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored
import Page.Lobby
import Page.SignIn
import Phoenix
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Socket as Socket exposing (Socket, AbnormalClose)
import Phoenix.Push as Push exposing (Push)
import Ports
import Route exposing (Route(..))
import Views.Lobby
import Views.Page
import Views.SignIn


-- MODEL --


type ChannelState
    = Joining
    | Joined
    | Left


type ConnectionStatus
    = Connecting
    | Connected ChannelState
    | Disconnected


type alias Flags =
    { token : Maybe String
    , socketUrl : String
    }


type Page
    = BlankPage
    | SignInPage Page.SignIn.Model
    | NotFoundPage
    | LobbyPage Page.Lobby.Model
    | ErroredPage Page.Errored.PageLoadError


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { pageState : PageState
    , session : Session
    , connectionStatus : ConnectionStatus
    , flags : Flags
    }


initialPage : Page
initialPage =
    BlankPage


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( model, cmd ) =
            setRoute (Route.fromLocation location)
                { pageState = Loaded initialPage
                , session = { user = Nothing }
                , connectionStatus = Connecting
                , flags = flags
                }
    in
        model ! [ cmd ]



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | ConnectionStatusChanged ConnectionStatus
    | HandleAdminChannelJoin Decode.Value
    | ViewsPageMsg Views.Page.Msg
    | PageSignInMsg Page.SignIn.Msg
    | PageLobbyMsg Page.Lobby.Msg
    | HandleSignOutSuccess Decode.Value


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute ({ connectionStatus, session, flags } as model) =
    let
        transition init page toMsg =
            let
                ( subModel, subCmd ) =
                    init
            in
                { model | pageState = TransitioningFrom (page subModel) } ! [ Cmd.map toMsg subCmd ]

        errored =
            pageErrored model
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFoundPage } ! [ Cmd.none ]

            Just SignInRoute ->
                { model | pageState = Loaded (SignInPage Page.SignIn.initialModel) } ! [ Cmd.none ]

            Just LobbyRoute ->
                case connectionStatus of
                    Disconnected ->
                        model ! [ Route.newUrl SignInRoute ]

                    _ ->
                        transition (Page.Lobby.init flags.socketUrl) LobbyPage PageLobbyMsg


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageErrored : Model -> String -> ( Model, Cmd msg )
pageErrored model errorMessage =
    let
        error =
            Page.Errored.pageLoadError errorMessage
    in
        { model | pageState = Loaded (ErroredPage error) } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ flags, pageState } as model) =
    updatePage flags (getPage pageState) msg model


updatePage : Flags -> Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage { socketUrl } page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( ViewsPageMsg subMsg, _ ) ->
                case subMsg of
                    Views.Page.SignOut ->
                        model
                            ! [ Push.init "admin:lobby" "sign_out"
                                    |> Push.onOk HandleSignOutSuccess
                                    |> Phoenix.push socketUrl
                              ]

            ( PageSignInMsg subMsg, SignInPage subModel ) ->
                toPage SignInPage PageSignInMsg Page.SignIn.update subMsg subModel

            ( PageLobbyMsg subMsg, LobbyPage subModel ) ->
                toPage LobbyPage PageLobbyMsg Page.Lobby.update subMsg subModel

            ( ConnectionStatusChanged connectionStatus, _ ) ->
                let
                    cmds =
                        case connectionStatus of
                            Disconnected ->
                                [ Route.newUrl SignInRoute
                                , Ports.saveToken Nothing
                                ]

                            _ ->
                                []
                in
                    { model | connectionStatus = connectionStatus } ! cmds

            ( HandleAdminChannelJoin payload, _ ) ->
                case Decode.decodeValue Data.Session.decoder payload of
                    Ok user ->
                        { model
                            | session =
                                { user = Just user
                                }
                            , connectionStatus = Connected Joined
                        }
                            ! [ Ports.saveToken model.flags.token ]

                    Err error ->
                        let
                            _ =
                                Debug.log "Error" error
                        in
                            model ! []

            ( HandleSignOutSuccess _, _ ) ->
                { model
                    | connectionStatus = Disconnected
                    , session = { user = Nothing }
                }
                    ! [ Route.newUrl SignInRoute, Ports.saveToken Nothing ]

            ( _, NotFoundPage ) ->
                model ! []

            ( _, _ ) ->
                model ! []



-- SUBSCRIPTIONS --


socket : Flags -> Socket Msg
socket { socketUrl, token } =
    Socket.init socketUrl
        |> Socket.withParams [ ( "token", Maybe.withDefault "" token ) ]
        |> Socket.onOpen (ConnectionStatusChanged (Connected Joining))
        |> Socket.onClose (\_ -> ConnectionStatusChanged Disconnected)
        |> Socket.withDebug


socketSubscriptions : Model -> Sub Msg
socketSubscriptions { connectionStatus, pageState, flags } =
    let
        { token } =
            flags

        adminChannel =
            Channel.init "admin:lobby"
                |> Channel.onJoin HandleAdminChannelJoin
                |> Channel.withDebug
    in
        case connectionStatus of
            Disconnected ->
                Sub.none

            _ ->
                Phoenix.connect (socket flags) [ adminChannel ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ socketSubscriptions model ]



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        header =
            Html.map ViewsPageMsg Views.Page.headerView

        frame =
            Views.Page.frame isLoading session.user header
    in
        case ( page, session.user ) of
            ( SignInPage subModel, _ ) ->
                map PageSignInMsg (Views.SignIn.view subModel)

            ( LobbyPage subModel, Just user ) ->
                map PageLobbyMsg (Views.Lobby.view user subModel) |> frame

            _ ->
                frame <| Html.text "View not implemented yet"



-- MAIN --


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
