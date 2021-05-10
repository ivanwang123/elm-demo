module Page.CountryInfo exposing (..)

import Data.Country exposing (..)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData exposing (WebData)
import Session exposing (Session)
import String exposing (toInt)



-- Model


type alias Model =
    { session : Session
    , country : WebData CountryLong
    }


type Msg
    = CountriesResponse (WebData CountryLong)



-- Init


init : String -> Session -> ( Model, Cmd Msg )
init alphaCode session =
    let
        initialModel =
            { session = session
            , country = RemoteData.Loading
            }
    in
    ( initialModel, getCountry alphaCode )


toSession : Model -> Session
toSession model =
    model.session



-- Api


getCountry : String -> Cmd Msg
getCountry alphaCode =
    Http.get
        { url = "https://restcountries.eu/rest/v2/alpha/" ++ alphaCode
        , expect = Http.expectJson (RemoteData.fromResult >> CountriesResponse) countryLongDecoder
        }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountriesResponse res ->
            let
                _ =
                    log "res" res
            in
            ( { model | country = res }, Cmd.none )



-- View


view : Model -> { title : String, content : Html Msg }
view model =
    let
        countryName =
            case model.country of
                RemoteData.Success country ->
                    country.name

                _ ->
                    "Country Info"
    in
    { title = countryName
    , content =
        div [ class "country-info-page" ]
            [ viewCountryOrError model ]
    }


viewCountryOrError : Model -> Html Msg
viewCountryOrError model =
    case model.country of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "center-text-page" ] [ text "Loading..." ]

        RemoteData.Success country ->
            viewCountry country

        -- TODO: Improve error messages
        RemoteData.Failure error ->
            let
                _ =
                    log "err" error
            in
            text "Error"


viewCountry : CountryLong -> Html Msg
viewCountry country =
    div [ class "main-content" ]
        [ nav []
            [ a [ href "/#" ] [ text "← Back" ] ]
        , section
            [ class "general-section" ]
            [ h1 [ class "name" ] [ text (country.name ++ " (" ++ country.alpha3Code ++ ")") ]
            , h3 [ class "alt-names" ] [ text (String.join ", " country.altSpellings) ]
            , img [ class "flag", src country.flag ] []
            ]
        , section [ class "info-section" ]
            [ p [] [ span [] [ text "Capital: " ], text country.capital ]
            , p [] [ span [] [ text "Population: " ], text (humanizeNumber country.population) ]
            , p [] [ span [] [ text "Demonym: " ], text country.demonym ]
            , p [] [ span [] [ text "Languages: " ], text (String.join ", " country.languages) ]
            , p [] [ span [] [ text "Currencies: " ], text (String.join ", " country.currencies) ]
            , p [] [ span [] [ text "Region: " ], text (country.subregion ++ ", " ++ country.region) ]
            , p [] [ span [] [ text "Area: " ], text (humanizeNumber (round country.area) ++ " km"), sup [] [ text "2" ] ]
            , p [] [ span [] [ text "Timezones: " ], text (String.join ", " country.timezones) ]
            , p [] [ span [] [ text "Gini: " ], text country.gini ]
            ]
        ]


humanizeNumber : Int -> String
humanizeNumber num =
    let
        numStr =
            String.fromInt num

        splitNum : String -> List String
        splitNum value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> splitNum
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    splitNum numStr
        |> List.reverse
        |> String.join ","
