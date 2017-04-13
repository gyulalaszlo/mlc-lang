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
import Error
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
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
                (model, Cmd.none)
--                updateWithOp
--                    (\traits i s -> traits.remove i s) model
--                    |> orNothing

            OpAppend scope ->
                doAppend model.traits model.path scope model.data
                    |> fromOpResult
--                    |> Result.map (\{cursor,new} ->
--                        ({ model | data = new, cursor = cursor }, Cmd.none))
--                    |> Result.mapError (\err ->
--                        ({ model | errors = err :: model.errors }, Cmd.none))
--                    |> Result.
--                (model, Cmd.none)
--                updateWithOp
--                    (\traits i s ->
--                        mapChild model.traits (\ts i s ->
--                            ts.append scope s
--                                |> Maybe.map (\(i,s) ->  ([i], s))
--                            ) i s
--                        ) model
--                    |> orNothing



doAppend : ScopeLikeTraits k s i d -> Path i -> s -> s -> OpResult s i
doAppend traits path new s =
    case path of
        [] -> Scopes.append traits new s
        i :: is -> Scopes.update traits i (doAppend traits is new) s


-- SCOPE STEPPING ---------------------------------
--
--
--{-| Shortcut for getting a child scope for a key
---}
--childAt: ScopeLikeTraits k s i d -> i -> s -> Maybe (List i, s)
--childAt traits i scope =
--    let
--        ts = scopeTraitsFor traits scope
--    in
--        ts.childScopeAt i scope
--            |> Maybe.map (\s -> ([i],s))
--
--
--{-|
---}
--mapChild: ScopeLikeTraits k s i d -> (ScopeTraits k s i d -> i -> s -> Maybe (List i, s)) -> i -> s -> Maybe (List i, s)
--mapChild treeTraits fn i scope =
--    let traits = scopeTraitsFor treeTraits scope
--    in
--        traits.childScopeAt i scope
--            |> Maybe.andThen (fn traits i)
--            |> Maybe.andThen (\(ps, new) ->
--                traits.replace i new scope
--                    |> Maybe.map (\ss -> (i :: ps, ss) )
--                )
----            |> Maybe.map (\ss -> ([i], ss))
--
--
--{-| Applies the function to the scope pointed to by the tip of `path`
---}
--mapPathHead : ScopeLikeTraits k s i d -> (ScopeTraits k s i d -> s -> Maybe (List i, s)) -> List i -> s -> Maybe (List i,s)
--mapPathHead traits fn path s =
--    let ts = scopeTraitsFor traits s
--    in case path of
--        [] -> fn ts s
--        i :: is ->
--            ts.childScopeAt i s
--                |> Maybe.andThen (\child -> mapPathHead traits fn is child)
--                |> Maybe.andThen (\(is,ss) -> ts.replace i ss s)
--


--mapPathTail :

{-

tree ops:

removeAt path (needs parent)
replaceAt path with (needs parent)

appendTo path what (needs head)

-}


--appendToPath: ScopeLikeTraits k s i d -> Path i -> s -> s -> OpResult k s
--appendToPath traits p new s  =
--    let err i = Error.makeMsg ["Cannot find child at", toString i]
--    in case p of
--        [] -> ((scopeTraitsFor traits s) |> .append) new s
--        i :: is ->
--            traits.childScopeAt i s
--                |> Result.fromMaybe (err i)
--                |> Result.andThen (appendToPath traits is new)
--                |> Result.andThen (\{key, new} -> traits.replace )



--
--
--
--
--
--{-| update the sarget of the current path
---}
--updateWithOp : (ScopeTraits k s i d -> i -> s -> Maybe (List i, s)) -> Model k s i d  -> Maybe (Model k s i d, Cmd (Msg s i))
--updateWithOp op model =
--    updateWithOpHelper op model model.path model.data
--        |> Maybe.map (\(newPath, newData) -> { model | data = newData, path = newPath})
--        |> Maybe.map (\m -> (m, Cmd.none))
--
--
--
--updateWithOpHelper : (ScopeTraits k s i d -> i -> s -> Maybe (List i, s)) -> Model k s i d -> List i -> s -> Maybe (List i, s)
--updateWithOpHelper op model path current =
--
--    let traits = scopeTraitsFor model.traits current
--        recurse pathRest childScope = updateWithOpHelper op model pathRest childScope
--        pathOrEmpty p = Maybe.map (\l -> [l]) p |> Maybe.withDefault []
--
--
--    in case path of
--        -- cannot operate on root
--        [] -> Nothing
--        -- single key means this is our target
--        [pathHead] ->
--            op traits pathHead current
--        -- go deeper and update
--        pathHead :: pathRest ->
--            -- find the child if there is any
--            traits.childScopeAt pathHead current
--                -- recurse with the op on the child if there is any
--                |> Maybe.andThen (\childScope -> recurse pathRest childScope)
--                -- then replace the current scopes version with the updated one
--                |> Maybe.andThen (\(newPath, child) ->
--                    traits.replace pathHead child current
--                        |> Maybe.map (\s -> (pathHead :: newPath, s)))
--
--
--

-- CURSOR UPDATE ----------------------------------


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
