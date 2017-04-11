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

import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Task
import SEd.Scopes exposing (BasicOperation(AppendOperation, RemoveOperation, ReplaceOperation), ScopeTraits)
import SEd.Scopes.Model as Model exposing (sExprAt, scopeAndTraitsForPath, scopeTraitsFor)
import SEd.Scopes.Msg exposing (Msg(..))
import SEd.Scopes.Views as Views


type alias ScopeLikeTraits k s i d =
    Model.ScopeLikeTraits k s i d


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
                updateWithOp
                    (\traits i s ->
                        let pathOrEmpty p = Maybe.map (\l -> [l]) p |> Maybe.withDefault []
                        in traits.operateOnChildAt RemoveOperation i s
                            |> Maybe.map (\(newC, newS) -> (pathOrEmpty newC , newS) )) model
                    |> orNothing

            OpAppend scope ->
--                updateWithOp
--                    (\traits i s ->
--                        let child = traits.childScopeAt i s
--                            childTraits = Maybe.map (\c -> scopeTraitsFor model.traits c) child
--                        in childTraits
--                            |> Maybe.map (\childTraits -> childTraits.append scope child)
--                            |> Maybe.map (\(p,s) -> ([p], s))) model
----                        let pathOrEmpty p = Maybe.map (\l -> [l]) p |> Maybe.withDefault []
----                        in traits.operateOnChildAt RemoveOperation i s
----                            |> Maybe.map (\(newC, newS) -> (pathOrEmpty newC , newS) )) model
--                    |> orNothing
                (model, Cmd.none)
--                updateWithOp (AppendOperation scope) model
--                    |> orNothing


{-| try to step into the current context
-}
stepDown : Model k s i d -> Maybe ( Model k s i d, Cmd (Msg s i) )
stepDown model =
    scopeAndTraitsForPath model.path model
        |> Maybe.andThen (\( current, traits ) -> traits.childKeys current)
        |> Maybe.andThen List.head
        |> Maybe.map (\head -> ( model, Task.perform AddPath (Task.succeed head) ))



updateWithOp : (ScopeTraits k s i d -> i -> s -> Maybe (List i, s)) -> Model k s i d  -> Maybe (Model k s i d, Cmd (Msg s i))
updateWithOp op model =
    updateWithOpHelper op model model.path model.data
        |> Maybe.map (\(newPath, newData) -> { model | data = newData, path = newPath})
        |> Maybe.map (\m -> (m, Cmd.none))



{-| update With Op
-}
updateWithOpHelper : (ScopeTraits k s i d -> i -> s -> Maybe (List i, s)) -> Model k s i d -> List i -> s -> Maybe (List i, s)
updateWithOpHelper op model path current =

    let traits = scopeTraitsFor model.traits current
        recurse pathRest childScope = updateWithOpHelper op model pathRest childScope
        pathOrEmpty p = Maybe.map (\l -> [l]) p |> Maybe.withDefault []


    in case path of
        -- cannot operate on root
        [] -> Nothing
        -- single key means this is our target
        [pathHead] ->
            op traits pathHead current
        -- go deeper and update
        pathHead :: pathRest ->
            -- find the child if there is any
            traits.childScopeAt pathHead current
                -- recurse with the op on the child if there is any
                |> Maybe.andThen (\childScope -> recurse pathRest childScope)
                -- then replace the current scopes version with the updated one
                |> Maybe.andThen (\(newPath, child) ->
                    traits.replace pathHead child current
                        |> Maybe.map (\s -> (pathHead :: newPath, s)))








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



-- VIEW: view


{-| view
-}
view : Model k s i d -> Html (Msg s i)
view model =
    div [ class "scope-editor-view" ]
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
    String.join "\n"
        [ viewCss
        , Views.css
        ]
