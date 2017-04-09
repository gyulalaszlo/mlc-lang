module SEd.Keyboard.KeysTest exposing (main)
{-| Describe me please...
-}
import SEd.Keyboard.KeyboardView exposing (Model, Msg(..), initialModel, update, subscriptions, view, css)
import Html exposing (Html)
import Html.Attributes
import Task


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )
    -- ( initialModel, Task.perform INIT_MSG (Task.succeed MSG_DATA) )


withCss : String -> (model -> Html msg) -> model -> Html msg
withCss css view model =
    Html.div []
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css"]
            [ Html.text css ]

        , view model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = withCss css view
        , update = update
        , subscriptions = subscriptions
        }
