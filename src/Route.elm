module Route exposing (..)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | CountryInfo String
    | Register


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
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)



-- Helpers


routeToString : Route -> String
routeToString route =
    "#" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Home ->
            []

        CountryInfo alphaCode ->
            [ "country", alphaCode ]

        Register ->
            [ "register" ]

        NotFound ->
            [ "not-found" ]
