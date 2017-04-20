module Qnject.Client exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , toolbarView
    , css
    )
{-| Describe me please...
-}

import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Http
import Html.Attributes exposing (class)
import Qnject.Connection as Connection exposing (Connection)
import Qnject.QAppView
import Qnject.Qobject exposing (..)


-- MODEL


type alias Model =
    { connection: Connection
    , app: Maybe QApp
    , requestError: Maybe Http.Error
    , qnjectQAppView: Qnject.QAppView.Model


    }


initialModel : Model
initialModel =
    { connection = Connection.default
    , app = Nothing
    , requestError = Nothing
    , qnjectQAppView = Qnject.QAppView.initialModel

    }


-- MSG


type Msg
    = Noop

    | Refresh

    | OnGetObjectList (Result Http.Error QApp)


    | QnjectQAppViewMsg Qnject.QAppView.Msg







-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []
        Refresh -> (model, Http.send OnGetObjectList (getQobjects model.connection))
        OnGetObjectList (Ok ss) ->
            ({ model | app = Just ss }, Cmd.none)

        OnGetObjectList (Err err) ->
            ({model | requestError = Just err}, Cmd.none)

        QnjectQAppViewMsg m -> updateQnjectQAppView m model




{-| Updates sub component: Qnject.QAppView
-}
updateQnjectQAppView : Qnject.QAppView.Msg -> Model -> (Model, Cmd Msg)
updateQnjectQAppView m model =
    let
        (sm, sc) = Qnject.QAppView.update m model.qnjectQAppView
    in
        ({ model | qnjectQAppView = sm }, Cmd.map QnjectQAppViewMsg sc)





getQobjects : Connection -> Http.Request QApp
getQobjects connection =
    Http.get (Connection.url "/qwidgets" connection) decodeQApp

-- VIEW


view : Model -> Html Msg
view model =
    div [ class "Client-view" ]
        [ case model.requestError of
            Nothing -> text ""
            Just err -> div [ class "request-errors" ]
                            [ text <| toString err ]
        , case model.app of
            Nothing -> text "no app"
            Just app ->
                    Qnject.QAppView.view
                        { model = model.qnjectQAppView
                        , app = app
                        }
                        |> Html.map QnjectQAppViewMsg


        , Html.button [onClick Refresh] [ text "Refresh" ]
        ]


-- VIEW: toolbarView



{-| toolbar view
-}
toolbarView : Model -> Html Msg
toolbarView model =
    span
        [ class "client-toolbar-view" ]
        [ case model.requestError of
            Nothing -> text ""
            Just err ->
                span [ class "request-errors" ]
                    [ text <| toString err ]

        , Html.button [onClick Refresh] [ text "Refresh" ]
        ]

{-| CSS parts for toolbarView
-}
toolbarViewCss : String
toolbarViewCss = """
.toolbar-view {  }
"""






-- CSS

css : String
css = """
.Client-view {}


.address { display:inline-block; border-bottom: 0.1em dotted; font-style: italic; }
.address:before { content: "@"; }
""" ++ Qnject.QAppView.css