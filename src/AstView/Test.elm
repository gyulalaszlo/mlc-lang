module AstView.Test exposing (..)
{-| Describe me please...
|-}

-- VIEW

import AstView.Css
import AstView.Main exposing (Model, Msg(..), view, update, subscriptions, initialModel )
import CAsmSample
import Html
import Task


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform SetAssembly (Task.succeed CAsmSample.sample) )


wrapView model =
    Html.div []
        [ AstView.Css.css
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

