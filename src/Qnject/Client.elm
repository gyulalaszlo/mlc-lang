module Qnject.Client exposing
    ( SharedModel
    , initialSharedModel, initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
--    , toolbarView
    , css
    )
{-| Describe me please...
-}

import Bsp.Cursor
import Bsp.DefaultTheme
import Bsp.Model
import Bsp.Msg
import Bsp.Root
import Bsp.SplitView
import Bsp.Traits
import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Http
import Html.Attributes exposing (class)
import Qnject.Connection as Connection exposing (Connection)
import Qnject.ViewerEffects exposing (Effects(OpenObjectView))
import Qnject.Viewers exposing (bspTraits)
import Qnject.Viewers.QAppView
import Qnject.Qobject exposing (..)
import Qnject.Viewers.QObjectOverview
import Task




-- MODEL

type alias SharedModel =
    { connection: Connection
    , app: Maybe QApp
    , requestError: Maybe Http.Error


    , qnjectQAppView: Qnject.Viewers.QAppView.Model
    }


initialSharedModel : SharedModel
initialSharedModel =
    { connection = Connection.default
    , app = Nothing
    , requestError = Nothing
    , qnjectQAppView = Qnject.Viewers.QAppView.initialModel

    }

---

type alias Model =
    { bsp: BspModel
    }

initialModel : Model
initialModel =
    { bsp = Bsp.Model.modelFrom bspTraits initialSharedModel

    }




-- MSG

type alias BspViewMsg = Bsp.Msg.Msg Qnject.Viewers.Msg Qnject.Viewers.Model
type alias BspLocalModel = Bsp.Traits.LocalModel Qnject.Viewers.Msg Qnject.Viewers.Model SharedModel
type alias BspModel = Bsp.Model.Model Qnject.Viewers.Msg Qnject.Viewers.Model SharedModel Effects

type Msg
    = Refresh

    | OnGetObjectList (Result Http.Error QApp)
    | OnOpenObjectView Address

    | WrappedBspMsg BspViewMsg

--    | ChildrenMsg ChildMsg







-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions { bsp } =
    Sub.map WrappedBspMsg <| Bsp.Root.subscriptions bsp






-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
         WrappedBspMsg m -> updateBspRoot m model

         Refresh -> (model, Http.send OnGetObjectList (getQobjects model.bsp.shared.connection))

         OnGetObjectList (Err err) ->
            updateShared (\s -> { s | requestError =  Just err, app = Nothing }) model

         OnGetObjectList (Ok oo) ->
            updateShared (\s -> { s | app = Just oo }) model

         OnOpenObjectView address ->
            updateBspRoot
                (Bsp.Msg.SplitAt
                    Bsp.Cursor.CHead
                    Bsp.SplitView.Horizontal
                    (Qnject.Viewers.objectView (fromShared .connection model) address))
                model

--         _ -> model ! []

fromShared : (SharedModel -> v) -> Model -> v
fromShared fn m = Bsp.Model.getShared m.bsp |> fn

updateShared: (SharedModel -> SharedModel) -> Model -> (Model, Cmd Msg)
updateShared fn model =
    let newBsp = Bsp.Model.mapShared fn model.bsp
    in ( { model | bsp = newBsp} , Cmd.none )



onRequestError : Http.Error -> Model -> (Model, Cmd Msg)
onRequestError err model =
    updateShared (\s -> { s | requestError = Just err }) model


{-| Updates sub component: Bsp.Root
-}
updateBspRoot : BspViewMsg -> Model -> (Model, Cmd Msg)
updateBspRoot m model =
    let
        (sm, sc, se) = Bsp.Root.update m model.bsp
    in
        ({ model | bsp = sm }
        , Cmd.batch
            [ Cmd.map WrappedBspMsg sc
            , case se of
                Nothing -> Cmd.none
                Just (OpenObjectView addr) ->
                    Task.perform OnOpenObjectView <| Task.succeed addr
            ]
        )



{-| Updates sub component: Bsp.Root
-}
view : Model -> Html Msg
view model =
    div
        [ class "qnject-main-view" ]
        [ toolbarView model
        , Html.map WrappedBspMsg <| Bsp.Root.view model.bsp
        ]






getQobjects : Connection -> Http.Request QApp
getQobjects connection =
    Http.get (Connection.url "/qwidgets" connection) decodeQApp


-- VIEW


{-| toolbar view
-}
toolbarView : Model -> Html Msg
toolbarView model =
    let shared = model.bsp.shared
    in span
        [ class "client-toolbar-view" ]
        [ case shared.requestError of
            Nothing -> text ""
            Just err ->
                span [ class "request-errors" ]
                    [ text <| toString err ]

        , Html.button [onClick Refresh] [ text <| "Refresh " ++ shared.connection.url ]
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


body.normal { font: 1em/1.2em "Fira Code", Monaco, "Courier New"; }
body { font: 14px/1.3em "Fira Code", Monaco, "Courier New"; }

.fill-area,
.client-toolbar-view,
.bsp-root-view,
.qnject-main-view { position: absolute; top: 0; bottom: 0; right: 0; left: 0;}


.toolbar-offset-top,
.bsp-root-view { top: 1.6em; }

.toolbar-top,
.client-toolbar-view { bottom: auto; height: 1em; padding: 0.3em;}


.address { display:inline-block; border-bottom: 0.2em dotted; font-style: italic; cursor: pointer; }
.address:hover { border-bottom-style: solid; }


""" ++ Qnject.Viewers.css


-- CHILDREN VIEWS---------------------------------------------------------------



