module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | CountryInfo String
    | Register
    | Login
    | Logout


parseUrl : Url -> Route
parseUrl url =
    let
        newUrl =
            { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    in
    case parse routeParser newUrl of
        Just route ->
            route

        Nothing ->
            NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map CountryInfo (s "country" </> string)
        , map Register (s "register")
        , map Login (s "login")
        , map Logout (s "logout")
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


privateRoute : Session -> Cmd msg -> Cmd msg
privateRoute session defaultCmd =
    if Session.isAuthenticated session then
        defaultCmd

    else
        replaceUrl (Session.navKey session) Login


unPrivateRoute : Session -> Cmd msg -> Cmd msg
unPrivateRoute session defaultCmd =
    if Session.isAuthenticated session then
        replaceUrl (Session.navKey session) Home

    else
        defaultCmd



-- Helpers


routeToString : Route -> String
routeToString route =
    "#" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        NotFound ->
            [ "not-found" ]

        Home ->
            []

        CountryInfo alphaCode ->
            [ "country", alphaCode ]

        Register ->
            [ "register" ]

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]
