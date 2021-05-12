module Page.Register exposing (..)

import Api
import Api.Endpoint as Endpoint
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (string)
import Json.Encode as E
import Route
import Session exposing (Session, userDecoder)



-- Model


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


type alias Form =
    { email : String
    , username : String
    , password : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | CompletedRegister (Result (List String) Session.User)
    | GotSession Session



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , problems = []
            , form =
                { email = ""
                , username = ""
                , password = ""
                }
            }
    in
    ( initialModel, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            log "update" "update"
    in
    case msg of
        SubmittedForm ->
            let
                _ =
                    log "submit" model.form
            in
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , register validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Ok user) ->
            ( model, Session.store user )

        CompletedRegister (Err errors) ->
            let
                serverErrors =
                    List.map ServerError errors
            in
            ( { model | problems = List.append model.problems serverErrors }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- Validation


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "Username can't be blank" ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "Email can't be blank" ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "Password can't be blank" ]

                else if String.length form.password < 8 then
                    [ "Password must be at least 8 characters long" ]

                else
                    []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- Api


register : TrimmedForm -> Cmd Msg
register (Trimmed form) =
    let
        user =
            E.object
                [ ( "username", E.string form.username )
                , ( "email", E.string form.email )
                , ( "password", E.string form.password )
                ]

        body =
            E.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Api.register body Session.userDecoder CompletedRegister



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- View


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div
            []
            [ h1 [] [ text "Sign Up" ]
            , viewForm model.form
            , ul
                []
                (List.map viewProblem model.problems)
            ]
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ class "flex flex-col", onSubmit SubmittedForm ]
        [ input
            [ placeholder "Username"
            , onInput EnteredUsername
            , value form.username
            ]
            []
        , input
            [ placeholder "Email"
            , onInput EnteredEmail
            , value form.email
            ]
            []
        , input
            [ placeholder "Password"
            , onInput EnteredPassword
            , value form.password
            ]
            []
        , button [] [ text "Sign Up" ]
        ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMsg =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMsg ]
