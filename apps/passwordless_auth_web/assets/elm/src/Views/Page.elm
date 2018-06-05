module Views.Page
    exposing
        ( frame
        , headerView
        , Msg(..)
        )

import Data.Session exposing (User)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


type Msg
    = SignOut


frame : Bool -> Maybe User -> Html msg -> Html msg -> Html msg
frame isLoading user header content =
    case user of
        Nothing ->
            Html.text ""

        Just user ->
            Html.div
                [ Html.class "main-section flex-1 flex-col flex h-screen" ]
                [ header
                , Html.div
                    [ Html.class "main-content bg-grey-lightest flex-1 flex items-center justify-center" ]
                    [ content ]
                ]


headerView : Html Msg
headerView =
    Html.header
        [ Html.class "main-header" ]
        [ Html.nav
            [ Html.class "flex justify-between" ]
            [ Html.span
                [ Html.class "flex-1 p-4 text-white text-left" ]
                [ Html.text "Admin panel" ]
            , Html.a
                [ Html.class "p-4"
                , Html.onClick SignOut
                ]
                [ Html.text "Sign out" ]
            ]
        ]
