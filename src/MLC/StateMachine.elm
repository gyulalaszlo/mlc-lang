module MLC.StateMachine exposing (..)
{-| Describe me please...
|-}

import List.Extra



type alias Transition msg state =
    { on: (msg -> Bool)
    , from: (state -> Bool)
    , with: (msg -> state -> (state, Cmd msg))
    }


type alias StateMachine msg state =
    { state: state
    , transitions: List (Transition msg state)
    }

{-| Creates a new state machine.
-}
stateMachine : state -> List (Transition msg state) -> StateMachine msg state
stateMachine s ts =
    { state = s, transitions = ts }


{-| Returns the current state of the state machine. Useful for views.
-}
state : StateMachine msg state -> state
state s = s.state



transition : msg -> StateMachine msg state -> (StateMachine msg state, Cmd msg)
transition msg sm =
    List.Extra.find (\t -> (t.from sm.state) && (t.on msg)) sm.transitions
        |> Maybe.map (\t ->
            let (cm,cc) = t.with msg sm.state
            in ({ sm | state = cm }, cc))
        |> Maybe.withDefault (sm, Cmd.none)




transitionThen : (msg -> (state, Cmd msg) -> (state, Cmd msg)) -> msg -> StateMachine msg state -> (StateMachine msg state, Cmd msg)
transitionThen fn msg sm =
    List.Extra.find (\t -> (t.from sm.state) && (t.on msg)) sm.transitions
        |> Maybe.map (\t ->
            let (cm,cc) = fn msg <| t.with msg sm.state
            in ({ sm | state = cm }, cc))
        |> Maybe.withDefault (sm, Cmd.none)



-- Generic State machine predicates

isInAnyState : state -> Bool
isInAnyState _ = True

isInState : (model -> state) -> state -> model -> Bool
isInState f s m = (f m) == s



