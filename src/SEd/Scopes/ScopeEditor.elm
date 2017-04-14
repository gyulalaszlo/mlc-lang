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
import Error
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Regex
import Task
import SEd.Scopes as Scopes exposing (OpResult, Path, ScopeTraits, scopeTraitsFor)
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

    in
        case msg of
            AddPath i ->
                { model | path = model.path ++ [ i ] } ! []

            SetPath is ->
                { model | path = is } ! []

            Up ->
                List.Extra.init model.path
                    |> Maybe.map (\p -> { model | path = p } ! [])
                    |> orNothing

            Down ->
                stepDown model |> orNothing

            Left ->
                updateStep .stepLeft model |> orNothing

            Right ->
                updateStep .stepRight model |> orNothing

            OpRemove ->
                doRemove model.traits model.path model.data
                    |> fromOpResult
--                updateWithOp
--                    (\traits i s -> traits.remove i s) model
--                    |> orNothing

            OpAppend scope ->
                doAppend model.traits model.path scope model.data
                    |> fromOpResult



-- APPEND ----------------------------------------------------------------------

doAppend : ScopeLikeTraits k s i d -> Path i -> s -> s -> OpResult s i
doAppend traits path new s =
    case path of
        [] -> Scopes.append traits new s
        i :: is -> Scopes.update traits i (doAppend traits is new) s


-- REMOVE ----------------------------------------------------------------------

doRemove : ScopeLikeTraits k s i d -> Path i -> s -> OpResult s i
doRemove traits path s =
    case path of
        [] -> Error.err "Cannot remove the root"
        [i] -> Scopes.remove traits i s
        i :: is -> Scopes.update traits i (doRemove traits is) s

{-| try to step into the current context
-}
stepDown : Model k s i d -> Maybe ( Model k s i d, Cmd (Msg s i) )
stepDown model =
    scopeAndTraitsForPath model.path model
        |> Maybe.andThen (\( current, traits ) -> traits.childKeys current)
        |> Maybe.andThen List.head
        |> Maybe.map (\head -> ( model, Task.perform AddPath (Task.succeed head) ))



{-| try to step into the current context
-}
updatePath :
    (List i -> s -> ScopeTraits k s i d -> Maybe (List i))
    -> List i
    -> Model k s i d
    -> Maybe ( Model k s i d, Cmd (Msg s i) )
updatePath fn path model =
    let
        setPath : List i -> ( Model k s i d, Cmd (Msg s i) )
        setPath newPath =
            ( model, Task.perform SetPath (Task.succeed newPath) )
    in
        List.Extra.init path
            |> Maybe.andThen (\path -> scopeAndTraitsForPath path model)
            |> Maybe.andThen (\( current, traits ) -> fn path current traits)
            |> Maybe.map setPath


updateStep :
    (ScopeTraits k s i d -> i -> s -> Maybe i)
    -> Model k s i d
    -> Maybe ( Model k s i d, Cmd (Msg s i) )
updateStep stepFn model =
    let
        newPath ps =
            Maybe.map (\i -> List.reverse (i :: ps))

        updateInner i s traits =
            case List.reverse i of
                h :: ps ->
                    stepFn traits h s |> (newPath ps)

                _ ->
                    Nothing
    in
        updatePath updateInner model.path model


pathOrEmpty : Maybe i -> List i
pathOrEmpty p =
    Maybe.map (\l -> [l]) p
        |> Maybe.withDefault []


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
