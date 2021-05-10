module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | CountryInfo String


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
        ]
