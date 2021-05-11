port module Session exposing (..)

import Browser.Navigation as Nav
import Debug exposing (log)
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E



-- Types


type Session
    = LoggedIn Nav.Key User
    | Guest Nav.Key


type User
    = User Cred


type Cred
    = Cred String String



-- Helpers


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


fromUser : Nav.Key -> Maybe User -> Session
fromUser key maybeUser =
    case maybeUser of
        Just user ->
            LoggedIn key user

        Nothing ->
            Guest key


userDecoder : Decoder (Cred -> User)
userDecoder =
    D.succeed User


store : User -> Cmd msg
store (User credVal) =
    storeCredWith credVal


storeCredWith : Cred -> Cmd msg
storeCredWith (Cred username token) =
    let
        json =
            E.object
                [ ( "user"
                  , E.object
                        [ ( "username", E.string username )
                        , ( "token", E.string token )
                        ]
                  )
                ]

        _ =
            log "json" (E.encode 2 json)
    in
    storeCache
        (Just json)


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    userChanges (\maybeUser -> toMsg (fromUser key maybeUser)) userDecoder


userChanges : (Maybe user -> msg) -> Decoder (Cred -> user) -> Sub msg
userChanges toMsg decoder =
    onStoreChange (\val -> toMsg (decodeFromChange decoder val))


decodeFromChange : Decoder (Cred -> user) -> Value -> Maybe user
decodeFromChange decoder val =
    D.decodeValue (storageDecoder decoder) val
        |> Result.toMaybe


storageDecoder : Decoder (Cred -> user) -> Decoder user
storageDecoder decoder =
    D.field "user" (decoderFromCred decoder)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    D.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


credDecoder : Decoder Cred
credDecoder =
    D.succeed Cred
        |> Pipeline.required "username" D.string
        |> Pipeline.required "token" D.string


port storeCache : Maybe Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg
