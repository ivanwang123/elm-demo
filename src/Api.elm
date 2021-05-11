module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)



-- HTTP


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


post : Endpoint -> Http.Body -> Decoder a -> (WebData a -> msg) -> Cmd msg
post url body decoder onResponse =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> onResponse) decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }
