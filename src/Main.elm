module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CountryInfo as CountryInfo
import Page.Home as Home
import Route exposing (Route(..))
import Session exposing (Session)
import Url exposing (Url)


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


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


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
    case model of
        NotFoundPage _ ->
            notFoundView

        Redirect _ ->
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
