module SEd.Scopes.ScopeEditor
    exposing
        ( Model
        , modelFrom
        , subscriptions
        , update
        , view
        , css
        )

{-| Describe me please...
-}

import Color
import Css.Background
import Css.Bits as Css exposing (emptyTheme)
import Css.Font
import Css.Padding
import Css.Rectangle
import Css.Selector
import Css.Size
import Dict exposing (Dict)
import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Regex
import Task
import SEd.Scopes as Scopes exposing (BasicScope(ListScope, StringScope), OpResult, Path, ScopeTraits, scopeTraitsFor)
import SEd.Scopes.Model as Model exposing (sExprAt, scopeAndTraitsForPath)
import SEd.Scopes.Msg exposing (Msg(..))
import SEd.Scopes.Views as Views


type alias ScopeLikeTraits k s i d =
    Scopes.ScopeLikeTraits k s i d


type alias Model k s i d =
    Model.Model k s i d



-- MODEL


modelFrom : ScopeLikeTraits k s i d -> s -> Model k s i d
modelFrom =
    Model.from



-- MSG
-- SUBSCRIPTIONS


subscriptions : Model k s i d -> Sub (Msg s i)
subscriptions =
    Model.subscriptions



-- UPDATE


update : Msg s i -> Model k s i d -> ( Model k s i d, Cmd (Msg s i) )
update msg model =
    let
        orNothing =
            Maybe.withDefault ( model, Cmd.none )
        updateData newData =
            ({ model | data = newData }, Cmd.none)

        fromOpResult res =
            case res of
                Ok {cursor, new} ->
                        ({ model | data = new, path = cursor }, Cmd.none)
                Err err ->
                        ({ model | errors = err :: model.errors }, Cmd.none)
        fromCursorResult res =
            case res of
                Ok cs -> ({ model | path = cs }, Cmd.none)
                Err err -> ({ model | errors = err :: model.errors}, Cmd.none)

        fromResult res =
                Result.map (\p -> ({model | path = p }, Cmd.none)) res

    in
        case msg of
            AddPath i ->
                Scopes.stepIntoNth model.traits model.path i model.data
                    |> fromCursorResult

            SetPath is ->
                { model | path = is } ! []

            Up ->
                List.Extra.init model.path
                    |> Maybe.map (\p -> { model | path = p } ! [])
                    |> orNothing

            Down ->
                Scopes.stepDown model.traits model.path model.data
                    |> fromCursorResult

            Left ->
                Scopes.stepLeft model.traits model.path model.data
                    |> fromCursorResult

            Right ->
                Scopes.stepRight model.traits model.path model.data
                    |> fromCursorResult

            OpRemove ->
                Scopes.recursiveRemove model.traits model.path model.data
                    |> fromOpResult

            OpAppend scope ->
                Scopes.recursiveAppend model.traits model.path scope model.data
                    |> fromOpResult







-- VIEW: view


{-| main view for the scope editor
-}
view : Model k s i d -> Html (Msg s i)
view model =
    div [class "scope-editor-view" ]
        [ Views.toolbarView model
        , sExprAt model.traits model.path model.data
            |> Maybe.map (Views.sExpressionView model.traits)
            |> Maybe.withDefault (text "Cannot find SExpr at path")

        , div [] <| List.map (text << toString) model.errors
        ]


{-| CSS parts for view
-}
viewCss : String
viewCss =
    """
.scope-editor-view {  }
"""




css : String
css =
    String.join "\n\n"
        [ viewCss
        , Views.css
        ]
