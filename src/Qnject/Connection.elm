module Qnject.Connection exposing (..)
{-| Describe me please...
-}

import Dict exposing (Dict)
import Error exposing (Error)
import Http
import Task
import Qnject.Qobject exposing (Address, QApp, QObject, decodeQApp)

type alias Model =
    { url: String
    , app: Maybe QApp
    , objects: Dict Address QObject
    , requestError: Maybe Http.Error
    }


default : Model
default =
    { url = "http://localhost:8000"
    , app = Nothing
    , objects = Dict.empty
    , requestError = Nothing
    }

type Msg
    = ReloadAll
    | OnReloadApp (Result Http.Error QApp)


url : String -> Model -> String
url path {url} =
    url ++ "/api" ++ path


{-| Gets all widgets from the qnject server that
-}
allWidgets : (Result Http.Error QApp -> msg) -> Model -> Cmd msg
allWidgets msg connection =
    Http.send msg <|
        Http.get (url "/qwidgets" connection) decodeQApp



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Debug.log "update" <|
        case msg of
            ReloadAll ->
                (model, allWidgets OnReloadApp model)

            OnReloadApp (Ok app) ->
                ({ model | app = Just app, requestError = Nothing }, Cmd.none)

            OnReloadApp (Err err) ->
                ({ model | app = Nothing, requestError = Just err }, Cmd.none)
