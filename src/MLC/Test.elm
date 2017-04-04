module MLC.Test exposing (..)
{-| Describe me please...
|-}

import AstView.Css
import Helpers.Css
import Html
import MLC.StructureEditor exposing (Model, Msg(..), update, initialModel, subscriptions, view)



init : ( Model, Cmd Msg )
init =
    (initialModel, Cmd.none)
--    ( initialModel, Task.perform SetAssembly (Task.succeed CAsmSample.sample) )


wrapView model =
    Html.div []
        [ Helpers.Css.css
        , view model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = wrapView
        , update = update
        , subscriptions = subscriptions
        }
