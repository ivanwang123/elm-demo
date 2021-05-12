module Page.Home exposing (..)

import Api
import Api.Endpoint as Endpoint
import Data.Country as Country exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData exposing (RemoteData, WebData)
import Session exposing (Session)



-- Model


type alias Model =
    { session : Session
    , countries : WebData (List CountryShort)
    }


type Msg
    = CountriesResponse (WebData (List CountryShort))
    | GotSession Session



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , countries = RemoteData.Loading
            }
    in
    ( initialModel, getCountries )


toSession : Model -> Session
toSession model =
    model.session



-- Api


getCountries : Cmd Msg
getCountries =
    Api.get Endpoint.countries countriesDecoder CountriesResponse



-- Http.get
--     { url = "https://restcountries.eu/rest/v2/all?fields=name;alpha3Code;flag"
--     , expect = Http.expectJson (RemoteData.fromResult >> CountriesResponse) countriesDecoder
--     }
-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountriesResponse res ->
            ( { model | countries = res }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- View


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Countries"
    , content =
        div [ class "home-page" ]
            [ viewCountriesOrError model ]
    }


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
        [ a [ href ("/#country/" ++ country.alpha3Code) ]
            [ img [ src country.flag ] []
            , text country.name
            ]
        ]
