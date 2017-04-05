module MLC.Test exposing (..)
{-| Describe me please...
|-}

import AstView.Css
import Helpers.Css
import Html
import MLC.StructureEditor exposing (Msg, StateMachineModel, update, initialStateMachine, subscriptions, view)



init : ( StateMachineModel, Cmd Msg )
init =
    (initialStateMachine, Cmd.none)


wrapView model =
    Html.div []
        [ Helpers.Css.css
        , view model
        ]


main : Program Never StateMachineModel Msg
main =
    Html.program
        { init = init
        , view = wrapView
        , update = update
        , subscriptions = subscriptions
        }
