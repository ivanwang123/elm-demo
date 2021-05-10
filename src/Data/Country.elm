module Data.Country exposing (..)

import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Json.Decode.Pipeline as Pipeline
import String exposing (String)



-- Model


type alias CountryShort =
    { name : String
    , alpha3Code : String
    , flag : String
    }


type alias CountryLong =
    { name : String
    , alpha3Code : String
    , capital : String
    , population : Int
    , region : String
    , subregion : String
    , demonym : String
    , nativeName : String
    , altSpellings : List String
    , area : Float
    , gini : Maybe Float
    , timezones : List String
    , currencies : List String
    , languages : List String
    , flag : String
    }



-- Decoders


countriesDecoder : Decoder (List CountryShort)
countriesDecoder =
    Decode.list countryShortDecoder


countryShortDecoder : Decoder CountryShort
countryShortDecoder =
    Decode.succeed CountryShort
        |> Pipeline.required "name" string
        |> Pipeline.required "alpha3Code" string
        |> Pipeline.required "flag" string


countryLongDecoder : Decoder CountryLong
countryLongDecoder =
    Decode.succeed CountryLong
        |> Pipeline.required "name" string
        |> Pipeline.required "alpha3Code" string
        |> Pipeline.required "capital" string
        |> Pipeline.required "population" int
        |> Pipeline.required "region" string
        |> Pipeline.required "subregion" string
        |> Pipeline.required "demonym" string
        |> Pipeline.required "nativeName" string
        |> Pipeline.required "altSpellings" (list string)
        |> Pipeline.required "area" float
        |> Pipeline.required "gini" (Decode.nullable float)
        |> Pipeline.required "timezones" (list string)
        |> Pipeline.required "currencies" (list currencyDecoder)
        |> Pipeline.required "languages" (list languageDecoder)
        |> Pipeline.required "flag" string


type alias Currency =
    { name : String }


currencyDecoder : Decoder String
currencyDecoder =
    Decode.map .name <|
        (Decode.succeed Currency
            |> Pipeline.required "name" string
        )


type alias Language =
    { name : String }


languageDecoder : Decoder String
languageDecoder =
    Decode.map .name <|
        (Decode.succeed Currency
            |> Pipeline.required "name" string
        )
