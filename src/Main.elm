module Main exposing (..)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Page
import Page.Blank as Blank
import Page.CountryInfo as CountryInfo
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Route exposing (Route(..))
import Session exposing (Cred, Session, User)
import Url exposing (Url)



-- Model


type Model
    = NotFoundPage Session
    | Redirect Session
    | HomePage Home.Model
    | CountryInfoPage CountryInfo.Model
    | RegisterPage Register.Model
    | LoginPage Login.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | HomeMsg Home.Msg
    | CountryInfoMsg CountryInfo.Msg
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg
    | GotSession Session



-- Init


init : Maybe User -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUser url navKey =
    changeRouteTo (Route.parseUrl url)
        (Redirect (Session.fromUser navKey maybeUser))



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
            let
                _ =
                    log "url" "url changed"
            in
            changeRouteTo (Route.parseUrl url) model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( HomeMsg pageMsg, HomePage pageModel ) ->
            Home.update pageMsg pageModel
                |> updateWith HomePage HomeMsg model

        ( CountryInfoMsg pageMsg, CountryInfoPage pageModel ) ->
            CountryInfo.update pageMsg pageModel
                |> updateWith CountryInfoPage CountryInfoMsg model

        ( RegisterMsg pageMsg, RegisterPage pageModel ) ->
            Register.update pageMsg pageModel
                |> updateWith RegisterPage RegisterMsg model

        ( LoginMsg pageMsg, LoginPage pageModel ) ->
            Login.update pageMsg pageModel
                |> updateWith LoginPage LoginMsg model

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

        Route.Logout ->
            ( model, Session.logout )

        Route.Register ->
            Register.init session
                |> updateWith RegisterPage RegisterMsg model

        Route.Login ->
            Login.init session
                |> updateWith LoginPage LoginMsg model


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

        RegisterPage pageModel ->
            Register.toSession pageModel

        LoginPage pageModel ->
            Login.toSession pageModel


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFoundPage _ ->
            Sub.none

        Redirect _ ->
            let
                _ =
                    log "subscriptions" "redirect"
            in
            Session.changes GotSession (Session.navKey (toSession model))

        HomePage pageModel ->
            Sub.map HomeMsg (Home.subscriptions pageModel)

        CountryInfoPage pageModel ->
            Sub.map CountryInfoMsg (CountryInfo.subscriptions pageModel)

        RegisterPage pageModel ->
            Sub.map RegisterMsg (Register.subscriptions pageModel)

        LoginPage pageModel ->
            Sub.map LoginMsg (Login.subscriptions pageModel)



-- View


view : Model -> Document Msg
view model =
    let
        user =
            Session.toUser (toSession model)
    in
    case model of
        NotFoundPage _ ->
            Page.view user Page.Other NotFound.view

        Redirect _ ->
            Page.view user Page.Other Blank.view

        HomePage pageModel ->
            viewPage user Page.Other HomeMsg (Home.view pageModel)

        CountryInfoPage pageModel ->
            viewPage user Page.Other CountryInfoMsg (CountryInfo.view pageModel)

        RegisterPage pageModel ->
            viewPage user Page.Other RegisterMsg (Register.view pageModel)

        LoginPage pageModel ->
            viewPage user Page.Other LoginMsg (Login.view pageModel)


viewPage : Maybe User -> Page.Page -> (msg -> Msg) -> { title : String, content : Html msg } -> Document Msg
viewPage maybeUser page toMsg pageView =
    let
        { title, body } =
            Page.view maybeUser page pageView
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }



-- Main


main : Program Value Model Msg
main =
    application Session.userDecoder
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


application :
    Decoder (Cred -> user)
    ->
        { init : Maybe user -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application userDecoder app =
    let
        newInit flags url navKey =
            let
                maybeUser =
                    D.decodeValue D.string flags
                        |> Result.andThen (D.decodeString (Session.storageDecoder userDecoder))
                        |> Result.toMaybe
            in
            app.init maybeUser url navKey
    in
    Browser.application
        { init = newInit
        , onUrlChange = app.onUrlChange
        , onUrlRequest = app.onUrlRequest
        , subscriptions = app.subscriptions
        , update = app.update
        , view = app.view
        }
