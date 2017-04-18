module Bsp.Traits exposing (..)

{-| Describe me please...
-}

-- TRAITS ----------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor)
import Bsp.Msg exposing (Id, Msg)
import Bsp.SplitView exposing (SplitMeta)
import Error exposing (Error)
import Html exposing (Html)


type alias LocalModel msg local shared =
    { local : local
    , shared : shared
    , cursor : Cursor
    , id : Id
    , msg : msg -> Msg msg local
    }


type alias SharedModel shared =
    { shared : shared
    , cursor : Cursor -> Cursor
    , meta : SplitMeta Id
    }


type alias EmptyViewFn msg local shared =
    Cursor -> shared -> Html (Msg msg local)


type alias GlobalViewFn msg local shared =
    shared -> Result Error (LocalModel msg local shared) -> Html (Msg msg local)


type alias LocalViewFn msg local shared =
    LocalModel msg local shared -> Html (Msg msg local)


{-| Renders the leaf from the traits view function and the local model.
Potentially you can add toolbars here.
-}
type alias LeafViewFn msg local shared =
    LocalViewFn msg local shared -> LocalModel msg local shared -> Html (Msg msg local)


type alias SplitViewFn msg local shared =
    SharedModel shared -> Html (Msg msg local) -> Html (Msg msg local)


type alias NodeViewBaseTraits msg local shared =
    { empty : EmptyViewFn msg local shared
    , split : SplitViewFn msg local shared
    , leaf : LeafViewFn msg local shared
    }


type alias NodeViewModeTraits msg local shared =
    { normal : NodeViewBaseTraits msg local shared
    , selected : NodeViewBaseTraits msg local shared
    , global : GlobalViewFn msg local shared
    }


type alias ToolbarTraits msg local shared =
    { normal : NodeViewModeTraits msg local shared
    , layoutEditing : NodeViewModeTraits msg local shared
    }


type alias Traits msg local shared =
    { subscriptions : LocalModel msg local shared -> Sub msg
    , update : msg -> LocalModel msg local shared -> ( local, shared, Cmd (Msg msg local) )
    , view : LocalViewFn msg local shared
    , empty : Cursor -> shared -> Html (Msg msg local)
    , toolbars : ToolbarTraits msg local shared
    }
