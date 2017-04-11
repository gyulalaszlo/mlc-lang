module SEd.Scopes.ScopeTest exposing (main)
{-| Describe me please...
-}
import SEd.Scopes.ScopeEditor exposing (Model,  modelFrom, view, update, subscriptions, css)
import SEd.Scopes.Msg exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes
import Task
import SEd.Scopes.SExprScopes exposing (kindOf, traitsFor, Scope(..), empty)


initialModel =
    modelFrom
        { kindOf = kindOf
        , traitsFor = traitsFor
        , empty = empty
        }
        (EList [ EKey "if"
               , EList [EKey "=", EKey "i", EKey "len"]
               , EList [ EKey "i"]
               , EList [ EKey "len"]
                ])


init =
    ( initialModel, Cmd.none )


withCss : String -> (model -> Html msg) -> model -> Html msg
withCss css view model =
    Html.div []
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css"]
            [ Html.text css ]

        , view model
        ]


main =
    Html.program
        { init = init
        , view = withCss css view
        , update = update
        , subscriptions = subscriptions
        }
