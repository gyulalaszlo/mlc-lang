module Effects exposing (..)
{-| Describe me please...
-}


type alias Effects e = List e


none : Effects e
none = []


from : e -> Effects e
from e = [e]

map : (e -> f) -> Effects e -> Effects f
map = List.map


batch : List (Effects e) -> Effects e
batch = List.concat

concat : Effects e -> Effects e -> Effects e
concat = List.append

-- FOLDING ------------------------

type alias Return msg model effect = (model, Cmd msg, Effects effect)


type alias FoldState msg model effect =
    { model: model
    , cmds: List (Cmd msg)
    , effects: Effects effect
    }



initialFoldState model cmd =
    { model = model
    , cmds = if cmd == Cmd.none then [] else [cmd]
    , effects = none
    }

foldInto : Return m o e -> FoldState m o e -> FoldState m o e
foldInto (model, cmd, effects) state =
    { state | model = model, cmds = state.cmds ++ [cmd], effects = concat state.effects effects  }

toReturn : FoldState m o e -> Return m o e
toReturn { model, cmds, effects } = (model, Cmd.batch cmds, effects)



fold : (e -> model -> (model, Cmd msg, Effects f)) -> Return msg model e -> Return msg model f
fold fn (model, cmd, es) =
    let
        folder e state =
            foldInto (fn e model) state
    in
        List.foldl folder (initialFoldState model cmd) es
            |> toReturn

foldFinal : (e -> model -> (model, Cmd msg)) -> Return msg model e -> (model, Cmd msg)
foldFinal fn (model, cmd, es) =
    let
        foldInto (m,c) cc =
            (m, Cmd.batch [c, cc])

        folder e (model, cmd) =
            foldInto (fn e model) cmd
    in
        List.foldl folder (model, cmd) es
--mapReturn : (m -> o -> e -> Return mm oo ee) -> Return m o e -> Return mm oo ee

--handle : Return m o f

--type Propagation msg model e
--    = Resolved (model, Cmd msg)
--    | Bubbles (Effects e)
--
--
--
--handle : ((model, Cmd msg, Effects e) -> Propagation msg model f) -> Effects e -> model -> (model, Cmd msg, Effects f)
--handle fn e model =
--    case fn model e of
--        Resolved (mdl, cmd) -> (mdl, cmd, [])
----        Bubbles
--
--

