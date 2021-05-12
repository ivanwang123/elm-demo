module Page.Login exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Session exposing (Session)



-- Model


type alias Model =
    { session : Session }


type Msg
    = CompletedLogin
    | GotSession Session



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session }
    in
    ( initialModel, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedLogin ->
            let
                cred =
                    Session.Cred "myName" "token"

                user =
                    Session.User cred
            in
            ( model, Session.store user )

        GotSession session ->
            let
                _ =
                    log "got session" session
            in
            ( { model | session = session }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- View


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div []
            [ h1 [] [ text "Login" ]
            , button [ onClick CompletedLogin ] [ text "login" ]
            ]
    }
