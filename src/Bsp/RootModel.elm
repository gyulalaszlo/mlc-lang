module Bsp.RootModel
    exposing
        ( Model
        , LocalModel
        , ToolbarTraits
        , Traits
        , Msg(..)
        , Id
        , nextId
        , insertAt
        , modelFrom
        , localModelFor
        , LayoutEditingMode(..)
        )

{-| Describe me please...
-}

-- ID --------------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor(..))
import Bsp.SplitView exposing (Direction(..), Ratio(..), SplitMeta, SplitModel(..), binary, leaf, splitAtCursor)
import Error exposing (Error)
import Dict exposing (Dict)
import Html exposing (Html)


type alias Id =
    Int


nextId : Id -> Id
nextId old =
    old + 1



-- MODEL & CONSTRUCTORS --------------------------------------------------------


type alias LocalModel msg local shared =
    { local : local
    , shared : shared
    , cursor : Cursor
    , msg : msg -> Msg msg local
    }


type alias Model msg local shared =
    { shared : shared
    , locals : Dict Id local
    , traits : Traits msg local shared
    , rootView : SplitModel Id
    , cursor : Cursor
    , nextId : Id
    , layoutEditingMode : LayoutEditingMode
    }


type alias ToolbarTraits msg local shared =
    { split : (Cursor -> Cursor ) -> SplitMeta Id -> shared -> Html (Msg msg local)
    , splitLayoutEditing : (Cursor -> Cursor) -> SplitMeta Id -> shared -> Html (Msg msg local)
    , leafLayoutEditing : Cursor -> Id -> local -> shared -> Html (Msg msg local)
    , leafSelectedLayoutEditing : Cursor -> Id -> local -> shared -> Html (Msg msg local)
    , globalLayoutEditor : Cursor -> shared -> Html (Msg msg local)
    }


type alias Traits msg local shared =
    { subscriptions : LocalModel msg local shared -> Sub msg
    , update : msg -> LocalModel msg local shared -> ( local, shared, Cmd (Msg msg local) )
    , view : LocalModel msg local shared -> Html (Msg msg local)
    , empty : Cursor -> shared -> Html (Msg msg local)
    , toolbars : ToolbarTraits msg local shared
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
    }


{-| Returns Just a LocalModel for an Id and Cursor or Nothing if the id is not in the locals.
-}
localModelFor : Cursor -> Id -> Model m l s -> Maybe (LocalModel m l s)
localModelFor cursor id { locals, shared } =
    Dict.get id locals
        |> Maybe.map (\local -> LocalModel local shared cursor (ChildMsg id))



-- MSG -------------------------------------------------------------------------


type Msg msg local
    = ChildMsg Id msg
    | Select Cursor
    | SplitAt Cursor Direction local
    | SetLayoutEditingMode LayoutEditingMode
    | SetDirection Cursor Direction
    | SwapLR Cursor

    | Rotate Cursor Bsp.SplitView.RotateDirection



-- LAYOUT EDITING MODES --------------------------------------------------------


{-| What kind of layout editing mode are we in
-}
type LayoutEditingMode
    = NotEditingLayout
    | EditingLayoutBlocks



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
