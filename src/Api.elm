module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http
import Json.Decode as D exposing (Decoder, field)
import RemoteData exposing (WebData)
import Session



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


post : Endpoint -> Http.Body -> Decoder a -> (Result (List String) a -> msg) -> Cmd msg
post url body decoder onResponse =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = expectJson onResponse decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


register : Http.Body -> Decoder (Session.Cred -> a) -> (Result (List String) a -> msg) -> Cmd msg
register body decoder onResponse =
    post Endpoint.register body (D.field "user" (Session.decoderFromCred decoder)) onResponse



-- Errors


expectJson : (Result (List String) a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \res ->
            case res of
                Http.BadStatus_ _ body ->
                    Result.Err
                        (body
                            |> D.decodeString (field "errors" errorsDecoder)
                            |> Result.withDefault [ "Server Error" ]
                        )

                err ->
                    Result.Err [ "Server Error" ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    D.keyValuePairs (D.list D.string)
        |> D.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors
