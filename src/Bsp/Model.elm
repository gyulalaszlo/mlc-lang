module Bsp.Model
    exposing
        ( Model
        , nextId
        , insertAt
        , modelFrom
        , localModelFor
        , sharedModelFor
        , localModelAt
        , nodeViewBaseTraitsFor
        )

{-| Describe me please...
-}

-- ID --------------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor(..))
import Bsp.Msg exposing (Id, LayoutEditingMode(EditingLayoutBlocks, NotEditingLayout), Msg(ChildMsg))
import Bsp.Ratio exposing (Ratio)
import Bsp.SplitView exposing (Direction(..), SplitMeta, SplitModel(..), binary, leaf, splitAtCursor)
import Bsp.Traits exposing (LocalModel, NodeViewBaseTraits, SharedModel, Traits)
import Error exposing (Error)
import Dict exposing (Dict)
import Html exposing (Html)


nextId : Id -> Id
nextId old =
    old + 1



-- MODEL & CONSTRUCTORS --------------------------------------------------------


type alias Model msg local shared =
    { shared : shared
    , locals : Dict Id local
    , traits : Traits msg local shared
    , rootView : SplitModel Id
    , cursor : Cursor
    , nextId : Id
    , layoutEditingMode : LayoutEditingMode
    , selectedLeaf : Maybe Id
    }


{-| Creates a new Bsp Root View
-}
modelFrom : Traits m l s -> s -> Model m l s
modelFrom traits shared =
    { shared = shared
    , locals = Dict.empty
    , traits = traits
    , rootView = Bsp.SplitView.Empty
    , cursor = CHead
    , nextId = 0
    , layoutEditingMode = NotEditingLayout
    , selectedLeaf = Nothing
    }


{-| Returns Just a LocalModel for an Id and Cursor or Nothing if the id is not in the locals.
-}
localModelFor : Cursor -> Id -> Model m l s -> Maybe (LocalModel m l s)
localModelFor cursor id { locals, shared } =
    Dict.get id locals
        |> Maybe.map (\local -> LocalModel local shared cursor id (ChildMsg id))


localModelAt : Cursor -> Model m l s -> Result Error (LocalModel m l s)
localModelAt c model =
    let
        err id =
            Error.makeMsg [ "Cannot find model for id:", toString id, "for cursor:", toString c ]

        localModelForId id =
            localModelFor model.cursor id model
                |> Result.fromMaybe (err id)

        localView =
            Bsp.SplitView.valueAt model.cursor model.rootView
                |> Result.andThen localModelForId
    in
        localView


sharedModelFor : (Cursor -> Cursor) -> SplitMeta Id -> s -> SharedModel s
sharedModelFor cFn meta s =
    { shared = s, cursor = cFn, meta = meta }


{-|
-}
nodeViewBaseTraitsFor : Cursor -> Model m l s -> NodeViewBaseTraits m l s
nodeViewBaseTraitsFor c model =
    let
        toolbar =
            case model.layoutEditingMode of
                NotEditingLayout ->
                    model.traits.toolbars.normal

                EditingLayoutBlocks ->
                    model.traits.toolbars.layoutEditing
    in
        if model.cursor == c then
            toolbar.selected
        else
            toolbar.normal



-- CURSOR AND CURSOR OPERATIONS ------------------------------------------------


type alias ViewNode =
    SplitModel Id


insertLocal : l -> Model m l s -> Model m l s
insertLocal local model =
    let
        id =
            model.nextId
    in
        { model
            | nextId = nextId id
            , locals = Dict.insert id local model.locals
        }


insertAt : Cursor -> Direction -> Ratio -> l -> Model m l s -> Model m l s
insertAt cursor direction ratio local model =
    splitAtCursor direction ratio model.nextId cursor model.rootView
        |> Result.map (\( cc, newRoot ) -> { model | rootView = newRoot, cursor = cc })
        |> Result.map (insertLocal local)
        |> Result.withDefault model
