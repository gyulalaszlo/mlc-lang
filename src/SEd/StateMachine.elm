module SEd.StateMachine exposing
    ( StateMachine
    , Transition
    , UpdateChain
    , stateMachine

    , transition, transitionToChain

    , updateState
    , isInAnyState, isInState
    )
{-| Describe me please...
|-}

import Html
import List.Extra
import Update


-- MODELS

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





{-| Transitioning for update chains
-}
transitionToChain : msg -> StateMachine x msg state -> UpdateChain x msg state
transitionToChain msg sm =
    Update.unhandled msg sm |> transition


{-| Regular transition and allow chaining
-}
transition : UpdateChain x msg state -> UpdateChain x msg state
transition = Update.andThen runTransition


runTransition : msg -> StateMachine x msg state -> UpdateChain x msg state
runTransition msg sm =
    let
        state = sm.state
        handler = List.Extra.find (\t -> (t.from state) && (t.on msg)) sm.transitions
    in
        case handler of
            Nothing -> Update.unhandled msg sm
            Just t -> Update.map (\msg state  -> ({ sm | state = state }, Cmd.none) ) (t.with msg state)


-- Generic State machine predicates

isInAnyState : state -> Bool
isInAnyState _ = True

isInState : (model -> state) -> state -> model -> Bool
isInState f s m = (f m) == s


---- LEGACY UPDATE

{-| Updates the nested state using the provided update function
-}
updateState : (msg -> model -> (model, Cmd msg)) -> msg -> StateMachine x msg model ->  (StateMachine x msg model, Cmd msg )
updateState update msg model =
    let (sm,sc) = update msg model.state in { model | state = sm } ! [sc]



