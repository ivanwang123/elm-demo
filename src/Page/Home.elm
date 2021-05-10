module Page.Home exposing (..)

import Data.Country as Country exposing (..)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData exposing (RemoteData, WebData)



-- Model


type alias Model =
    { countries : WebData (List CountryShort) }


type Msg
    = CountriesResponse (WebData (List CountryShort))



-- Init


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { countries = RemoteData.Loading }
    in
    ( initialModel, getCountries )



-- Api


getCountries : Cmd Msg
getCountries =
    Http.get
        { url = "https://restcountries.eu/rest/v2/all?fields=name;alpha3Code;flag"
        , expect = Http.expectJson (RemoteData.fromResult >> CountriesResponse) countriesDecoder
        }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountriesResponse res ->
            let
                _ =
                    log "countries" res
            in
            ( { model | countries = res }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "home-page" ]
        [ viewCountriesOrError model ]


viewCountriesOrError : Model -> Html Msg
viewCountriesOrError model =
    case model.countries of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "center-text-page" ] [ text "Loading..." ]

        RemoteData.Success countries ->
            viewCountries countries

        RemoteData.Failure error ->
            text "Error"


viewCountries : List CountryShort -> Html Msg
viewCountries countries =
    div [ class "countries-section" ] (List.map viewCountry countries)


viewCountry : CountryShort -> Html Msg
viewCountry country =
    div [ class "country-container" ]
        [ a [ href ("/#/country/" ++ country.alpha3Code) ]
            [ img [ src country.flag ] []
            , text country.name
            ]
        ]
