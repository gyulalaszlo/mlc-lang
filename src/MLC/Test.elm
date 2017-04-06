module MLC.Test exposing (..)
{-| Describe me please...
|-}

import AstView.Css
import Helpers.Css
import Html
import MLC.StructureEditor exposing (Msg, StateMachineModel, initialStateMachine, subscriptions)
import SEd.Main.View exposing (view)
import SEd.Main.Update



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
        , update = SEd.Main.Update.update
        , subscriptions = subscriptions
        }
