module Api.Endpoint exposing (..)

import Http
import Json.Decode exposing (Decoder)
import Url.Builder exposing (QueryParameter, string)



-- Request


request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd a
request req =
    Http.request
        { body = req.body
        , expect = req.expect
        , headers = req.headers
        , method = req.method
        , timeout = req.timeout
        , url = toString req.url
        , tracker = req.tracker
        }



-- Types


type Endpoint
    = Endpoint String


toString : Endpoint -> String
toString (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "https://restcountries.eu/rest/v2" paths queryParams
        |> Endpoint



-- Endpoints


countries : Endpoint
countries =
    url [ "all" ] [ string "fields" "name;alpha3Code;flag" ]


country : String -> Endpoint
country alphaCode =
    url [ "alpha", alphaCode ] []
