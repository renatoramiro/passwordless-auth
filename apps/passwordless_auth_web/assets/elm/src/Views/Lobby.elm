module Views.Lobby exposing (view)

import Html exposing (Html, form)
import Html.Attributes as Html
import Data.Session exposing (User)
import Page.Lobby exposing (Model, Msg(..))


view : User -> Model -> Html Msg
view { email } model =
    Html.div
        []
        [ Html.img
            [ Html.src "/images/icons8-confetti-128.png"
            , Html.class "mb-4 jello-horizontal"
            ]
            []
        , Html.h3
            []
            [ Html.text <| "Welcome " ++ email ++ "!" ]
        , Html.p
            []
            [ Html.text "You signed in successfully." ]
        ]
