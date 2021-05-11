module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)



-- HTTP
-- TODO: Figure out Msg type


get : Endpoint -> Decoder a -> (WebData a -> msg) -> Cmd msg
get url decoder onResponse =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> onResponse) decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }



-- Http.expectJson (RemoteData.fromResult >> CountriesResponse) countryLongDecoder
