module MLC.StateMachine exposing (..)
{-| Describe me please...
|-}

import List.Extra



type alias Transition x msg state =
    { on: (msg -> Bool)
    , from: (state -> Bool)
    , with: (msg -> state -> Result x (state, Cmd msg))
    }


type alias StateMachine x msg state =
    { state: state
    , transitions: List (Transition x msg state)
    }

{-| Creates a new state machine.
-}
stateMachine : state -> List (Transition x msg state) -> StateMachine x msg state
stateMachine s ts =
    { state = s, transitions = ts }


{-| Returns the current state of the state machine. Useful for views.
-}
state : StateMachine x msg state -> state
state s = s.state



transition : msg -> StateMachine x msg state -> Res x msg state
transition msg sm =
    let handler = List.Extra.find (\t -> (t.from sm.state) && (t.on msg)) sm.transitions
    in case handler of
        Nothing -> Ok (sm, Cmd.none)
        Just t ->
            t.with msg sm.state
                |> Result.map (\(cm,cc) -> ({ sm | state = cm }, cc))


type alias Res x msg state = Result x (StateMachine x msg state, Cmd msg)
type alias TransitionThen x msg state =
    msg -> (state, Cmd msg) -> Result x (state, Cmd msg)


transitionThen : TransitionThen x m s  -> m -> StateMachine x m s -> Res x m s
transitionThen fn msg sm =
    let handler = List.Extra.find (\t -> (t.from sm.state) && (t.on msg)) sm.transitions
    in case handler of
        Nothing -> Ok (sm, Cmd.none)
        Just t ->
            t.with msg sm.state
                |> Result.andThen (fn msg)
                |> Result.map (\(cm, cc) -> ({ sm | state = cm }, cc))
--            let (cm,cc) = fn msg <|
--            in )
--        |> Maybe.withDefault (sm, Cmd.none)



-- Generic State machine predicates

isInAnyState : state -> Bool
isInAnyState _ = True

isInState : (model -> state) -> state -> model -> Bool
isInState f s m = (f m) == s



