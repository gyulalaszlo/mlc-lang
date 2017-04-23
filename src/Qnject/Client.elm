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
import Colors.Monokai
import Css
import Effects exposing (Effects)
import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Http
import Html.Attributes exposing (class)
import Qnject.Connection as Connection
import Qnject.ViewerEffects exposing (ViewerEffect(OpenObjectView))
import Qnject.Viewers exposing (bspTraits)
import Qnject.Viewers.QAppView
import Qnject.Qobject exposing (..)
import Qnject.Viewers.QObjectOverview
import Task




-- MODEL

type alias SharedModel =
    { connection: Connection.Model
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
type alias BspModel = Bsp.Model.Model Qnject.Viewers.Msg Qnject.Viewers.Model SharedModel ViewerEffect

type Msg
    = Refresh
    | ConnectionMsg Connection.Msg

    | OnGetObjectList (Result Http.Error QApp)
    | OnOpenObjectView Address

    | WrappedBspMsg BspViewMsg








-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions { bsp } =
    Sub.map WrappedBspMsg <| Bsp.Root.subscriptions bsp






-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let sharedUpdate fn = updateSharedModel fn model
    in case msg of
         WrappedBspMsg m -> updateBspRoot m model
         ConnectionMsg m -> updateShared (updateConnection m) model

         Refresh ->
            updateShared (updateConnection Connection.ReloadAll) model


         OnGetObjectList (Err err) ->
            sharedUpdate (\s -> { s | requestError =  Just err, app = Nothing })

         OnGetObjectList (Ok oo) ->
            sharedUpdate (\s -> { s | app = Just oo })

         OnOpenObjectView address ->
            updateBspRoot
                (Bsp.Msg.SplitAt
                    Bsp.Cursor.CHead
                    Bsp.SplitView.Horizontal
                    (Qnject.Viewers.objectView address))
                model

--         _ -> model ! []

fromShared : (SharedModel -> v) -> Model -> v
fromShared fn m = Bsp.Model.getShared m.bsp |> fn

updateSharedModel: (SharedModel -> SharedModel) -> Model -> (Model, Cmd Msg)
updateSharedModel fn model =
    let newBsp = Bsp.Model.mapShared fn model.bsp
    in ( { model | bsp = newBsp} , Cmd.none )

updateShared: (SharedModel -> (SharedModel, Cmd Msg)) -> Model -> (Model, Cmd Msg)
updateShared fn model =
    let bsp = model.bsp
        (newShared, cc) = fn bsp.shared
    in ({ model | bsp = { bsp | shared = newShared }}, cc)


onRequestError : Http.Error -> Model -> (Model, Cmd Msg)
onRequestError err model =
    updateSharedModel (\s -> { s | requestError = Just err }) model


{-| Updates sub component: Bsp.Root
-}
updateBspRoot : BspViewMsg -> Model -> (Model, Cmd Msg)
updateBspRoot m model =
    let
        (sm, sc, se) = Bsp.Root.update m model.bsp
        init = ({model | bsp = sm}, Cmd.map WrappedBspMsg sc, se)
    in
        Effects.foldFinal onEffects init
--        updateFromEffects { model | bsp = sm } se




{-| Updates sub component: Connection
-}
updateConnection : Connection.Msg -> SharedModel -> (SharedModel, Cmd Msg)
updateConnection m model =
    let
        (sm, sc) = Connection.update m model.connection
    in
        ({ model | connection = sm }, Cmd.map ConnectionMsg sc)




-- EFFECTS ---------------------------------------------------------------------

--updateFromEffects : Model -> Effects ViewerEffect -> (Model, Cmd Msg)
--updateFromEffects model effects =
--    effects
--        |> Effects.fold (onEffects model)
--        |> Maybe.withDefault (model, Cmd.none)

onEffects : ViewerEffect -> Model -> (Model, Cmd Msg)
onEffects effects model =
    case effects of
        OpenObjectView address ->
            (model , Cmd.batch
                [ sendMsg OnOpenObjectView address
                , sendMsg (ConnectionMsg << Connection.AddObject) address
                ])


sendMsg : (a -> Msg) -> a -> Cmd Msg
sendMsg msg a =
    Task.perform msg <| Task.succeed a


-- VIEW ------------------------------------------------------------------------

{-| Updates sub component: Bsp.Root
-}
view : Model -> Html Msg
view model =
    div
        [ class "qnject-main-view" ]
        [ toolbarView model
        , Html.map WrappedBspMsg <| Bsp.Root.view model.bsp
        ]







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
css =
    let themeColor cfn = (Css.color << cfn) Colors.Monokai.theme
    in Css.css
        [ ( "background-color", themeColor .background )
        , ( "text-color", themeColor .text )
        , ( "leaf-selection", themeColor .selection )
        , ( "split-background", "black" )
        , ( "leaf-radius", "5px" )
        , ( "leaf-margin", "0.1em" )

        , ( "color-darkbg", Css.color <| Colors.Monokai.darkBlack)

        -- layout-editing
        , ( "layout-border-size", "0.2em" )
        , ( "layout-margin", "0.3em" )
        , ( "layout-editing-margin", "0.3em" )

        , ( "layout-editing-split-border-style", "0.1em dotted" )
        -- toolbar data
        , ( "toolbar-margin", "0.3em" )
        , ( "toolbar-height", "1.6em" )

        , ( "font-size", "13px")
        , ( "line-height", "17px")

        , ("button-color", themeColor .selection )
        , ("button-background", "none")
        , ("button-border-color", themeColor .background )
        , ("button-hover-color", themeColor .background )
        , ("button-hover-background-color", themeColor .selection )
        , ("button-border-radius", "0.4em")

        , ("normal-font", "Trebuchet MS")
        , ("fixed-font", "\"Fira Code\", Monaco, \"Courier New\"")
        ]
        """

body { font: {{ font-size }}/{{ line-height }} {{ normal-font }}; }

code, pre { font-family: {{ fixed-font }}; }

.fill-area,
.client-toolbar-view,
.bsp-root-view,
.qnject-main-view { position: absolute; top: 0; bottom: 0; right: 0; left: 0;}


.toolbar-offset-top,
.bsp-root-view { top: {{ toolbar-height }}; }

.toolbar-top,
.client-toolbar-view { bottom: auto; height: {{ toolbar-height }}; }


.address { display:inline-block; border-bottom: 0.2em dotted; font-style: italic; cursor: pointer; }
.address:hover { border-bottom-style: solid; }

button { border: 0.2em solid {{ button-border-color }}; background: {{ button-background }}; color: {{ button-color }}; border-radius: {{ button-border-radius }}; outline: none; cursor: pointer; }
button:hover { background: {{ button-hover-background-color }}; color: {{ button-hover-color }}; }

""" ++ Qnject.Viewers.css


-- CHILDREN VIEWS---------------------------------------------------------------



