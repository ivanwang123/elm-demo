module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Page.Blank as Blank
import Page.CountryInfo as CountryInfo
import Page.Home as Home
import Page.NotFound as NotFound
import Route exposing (Route(..))
import Session exposing (Session)
import Url exposing (Url)



-- Model


type Model
    = NotFoundPage Session
    | Redirect Session
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
    changeRouteTo (Route.parseUrl url)
        (Redirect (Session.fromKey navKey))



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.parseUrl url) model

        ( HomeMsg pageMsg, HomePage pageModel ) ->
            Home.update pageMsg pageModel
                |> updateWith HomePage HomeMsg model

        ( CountryInfoMsg pageMsg, CountryInfoPage pageModel ) ->
            CountryInfo.update pageMsg pageModel
                |> updateWith CountryInfoPage CountryInfoMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Route.NotFound ->
            ( NotFoundPage session, Cmd.none )

        Route.Home ->
            Home.init session
                |> updateWith HomePage HomeMsg model

        Route.CountryInfo alphaCode ->
            CountryInfo.init alphaCode session
                |> updateWith CountryInfoPage CountryInfoMsg model


toSession : Model -> Session
toSession model =
    case model of
        NotFoundPage session ->
            session

        Redirect session ->
            session

        HomePage pageModel ->
            Home.toSession pageModel

        CountryInfoPage pageModel ->
            CountryInfo.toSession pageModel


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Document Msg
view model =
    case model of
        NotFoundPage _ ->
            Page.view Page.Other NotFound.view

        Redirect _ ->
            Page.view Page.Other Blank.view

        HomePage pageModel ->
            viewPage Page.Other HomeMsg (Home.view pageModel)

        CountryInfoPage pageModel ->
            viewPage Page.Other CountryInfoMsg (CountryInfo.view pageModel)


viewPage : Page.Page -> (msg -> Msg) -> { title : String, content : Html msg } -> Document Msg
viewPage page toMsg pageView =
    let
        { title, body } =
            Page.view page pageView
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }



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
