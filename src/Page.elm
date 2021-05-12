module Page exposing (..)

import Api
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route
import Session


type Page
    = Other


view : Maybe Session.User -> Page -> { title : String, content : Html msg } -> Document msg
view maybeUser page { title, content } =
    { title = title
    , body = viewHeader page maybeUser :: content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Session.User -> Html msg
viewHeader page maybeUser =
    nav [ class "text-blue-100 font-bold underline px-12 py-4" ]
        [ a [ Route.href Route.Home ] [ text "Home" ]
        , viewMenu page maybeUser
        ]


viewMenu : Page -> Maybe Session.User -> Html msg
viewMenu page maybeUser =
    case maybeUser of
        Just user ->
            let
                username =
                    Session.username user
            in
            div [ class "flex flex-col" ]
                [ text ("Hello " ++ username)
                , a [ Route.href Route.Logout ] [ text "Logout" ]
                ]

        Nothing ->
            div [ class "flex flex-col" ]
                [ a [ Route.href Route.Login ] [ text "Login" ]
                , a [ Route.href Route.Register ] [ text "Sign up" ]
                ]


viewFooter : Html msg
viewFooter =
    footer []
        [ text "footer" ]
