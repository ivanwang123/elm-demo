module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CountryInfo as CountryInfo
import Page.Home as Home
import Route exposing (Route)
import Url exposing (Url)



-- Model


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | HomePage Home.Model
    | CountryInfoPage CountryInfo.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | HomeMsg Home.Msg
    | CountryInfoMsg CountryInfo.Msg



-- Init


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        initialModel =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( initialModel, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, prevCmds ) =
    let
        ( curPage, curCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Home ->
                    let
                        ( pageModel, pageCmds ) =
                            Home.init
                    in
                    ( HomePage pageModel, Cmd.map HomeMsg pageCmds )

                Route.CountryInfo alphaCode ->
                    let
                        ( pageModel, pageCmds ) =
                            CountryInfo.init alphaCode
                    in
                    ( CountryInfoPage pageModel, Cmd.map CountryInfoMsg pageCmds )
    in
    ( { model | page = curPage }
    , Cmd.batch [ prevCmds, curCmds ]
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }
            , Cmd.none
            )
                |> initCurrentPage

        ( HomeMsg pageMsg, HomePage pageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    Home.update pageMsg pageModel
            in
            ( { model | page = HomePage updatedModel }
            , Cmd.map HomeMsg updatedCmd
            )

        ( CountryInfoMsg pageMsg, CountryInfoPage pageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    CountryInfo.update pageMsg pageModel
            in
            ( { model | page = CountryInfoPage updatedModel }
            , Cmd.map CountryInfoMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Document Msg
view model =
    { title = "My Elm App"
    , body =
        [ div [ class "container" ]
            [ pageView model ]
        ]
    }


pageView : Model -> Html Msg
pageView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        HomePage pageModel ->
            Home.view pageModel
                |> Html.map HomeMsg

        CountryInfoPage pageModel ->
            CountryInfo.view pageModel
                |> Html.map CountryInfoMsg


notFoundView : Html Msg
notFoundView =
    div [ class "center-text-page" ] [ text "Oops, page not found" ]



-- Main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
