module MLC.StateMachine exposing (..)
{-| Describe me please...
|-}

import Html
import List.Extra
import Update



--type alias InnerResult x msg state = Result x (state, Cmd msg)
--type alias InnerTransResult x msg state = Maybe (InnerResult x msg state)
--
--type alias OuterResult x msg state = Result x (StateMachine x msg state, Cmd msg)
type alias TransitionFn x msg state = msg -> state -> Update.Chain x msg state


type alias Transition x msg state =
    { on: (msg -> Bool)
    , from: (state -> Bool)
    , with: TransitionFn x msg state
    }


type alias StateMachine x msg state =
    { state: state
    , transitions: List (Transition x msg state)
    }

type alias UpdateChain x msg state =
    Update.Chain x msg (StateMachine x msg state)

{-| Creates a new state machine.
-}
stateMachine : state -> List (Transition x msg state) -> StateMachine x msg state
stateMachine s ts =
    { state = s, transitions = ts }


{-| Returns the current state of the state machine. Useful for views.
-}
state : StateMachine x msg state -> state
state s = s.state






transitionToChain : msg -> StateMachine x msg state -> UpdateChain x msg state
transitionToChain msg sm =
    Update.unhandled msg sm
        |> Update.andThen runTransition
--        |> Maybe.map (afterTransition sm)

transition : UpdateChain x msg state -> UpdateChain x msg state
transition = Update.andThen runTransition


--transitionThen : TransitionFn x msg s  -> msg -> StateMachine x msg s -> OuterResult x msg s
--transitionThen fn msg sm =
--    case runTransition msg sm of
--        Nothing -> Ok (sm, Cmd.none)
--        Just (Err e) -> Err e
--        Just (Ok (resultModel, resultCmd)) ->
--            let (thenModel, thenCmd) = fn msg resultModel
--            in afterTransition sm (thenModel, Cmd.batch [resultCmd, thenCmd])
----            |> Result.andThen (Maybe.map (\res -> fn msg res))



runTransition : msg -> StateMachine x msg state -> UpdateChain x msg state
runTransition msg sm =
    let
        state = sm.state
        handler = List.Extra.find (\t -> (t.from state) && (t.on msg)) sm.transitions
    in
        case handler of
            Nothing -> Update.unhandled msg sm
            Just t -> Update.map (\msg state  -> ({ sm | state = state }, Cmd.none) ) (t.with msg state)


--
--afterTransition : StateMachine x msg state -> InnerTransResult x msg state -> OuterResult x msg state
--afterTransition sm res =
--    case res of
--        Nothing -> Ok (sm, Cmd.none)
--        Just (Ok (cm,cc)) -> Ok <| { sm | state = cm } ! [cc]
--        Just (Err err) -> Err err

-- Generic State machine predicates

isInAnyState : state -> Bool
isInAnyState _ = True

isInState : (model -> state) -> state -> model -> Bool
isInState f s m = (f m) == s


---- LEGACY UPDATE
--
--type alias UpdateFn x s c n = Msg s c n ->  Model x s c n -> (Model x s c n, Cmd (Msg s c n))
--
--{-| Wraps updating the inner state of the state machin
---}
--updateState : UpdateFn x s c n -> Msg s c n -> SMModel x s c n -> (SMModel x s c n, Cmd (Msg s c n))
--updateState update msg model =
--    let (sm,sc) = update msg model.state in { model | state = sm } ! [sc]



updateState update msg model =
    let (sm,sc) = update msg model.state in { model | state = sm } ! [sc]




main = Html.text ""

