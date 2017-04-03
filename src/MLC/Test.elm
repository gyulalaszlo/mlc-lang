module MLC.Test exposing (..)
{-| Describe me please...
|-}

import Html
import MLC.StructureEditor exposing (Model, Msg(..), update, initialModel, subscriptions, view)



init : ( Model, Cmd Msg )
init =
    (initialModel, Cmd.none)
--    ( initialModel, Task.perform SetAssembly (Task.succeed CAsmSample.sample) )


--wrapView model =
--    Html.div []
--        [ AstView.Css.css
--        , view model
--        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
