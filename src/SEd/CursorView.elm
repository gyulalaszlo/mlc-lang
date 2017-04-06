module SEd.CursorView exposing
    ( Model
    , Traits

    , initialModel
    , setState

    , Msg(..)
    , subscriptions
    , update
    , view)
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import SEd.Operations exposing (ScopeMeta)

--import MLC.Cursor as Cursor exposing (Cursor)



-- MODEL

--type alias ScopeMeta =
--    { name: String
--    }


type alias Traits state cursor =
    { stateToString: state -> String
    , cursorToStringList: cursor -> List String
    , stateMeta: state -> ScopeMeta
    , initialState: state
--    , cursorForState: state -> cursor
    }

type alias Model state cursor =
    { states: List ScopeMeta
    , current: ScopeMeta
    , traits: Traits state cursor
    }


initialModel : Traits s c -> Model s c
initialModel traits =
    { states = []
    , current = traits.stateMeta traits.initialState
    , traits = traits
    }


setState : s -> List s -> Model s c -> Model s c
setState s ss model =
    { model
        | current = model.traits.stateMeta s
        , states = List.map model.traits.stateMeta ss
        }



-- MSG


type Msg s c
    = PushState s
    | PopState
    | SetCursor c



-- SUBSCRIPTIONS


subscriptions : Model s c -> Sub (Msg s c)
subscriptions model =
    Sub.none



-- UPDATE


update : Msg s c -> Model s c -> (Model s c, Cmd (Msg s c))
update msg model =
    case msg of
        _ -> (model, Cmd.none)


-- VIEW


view : Model s c -> Html (Msg s c)
view model =
    case model.states of
        [] -> noCursorView model
        _ -> hasCursorView model.states model


stackScopesDiv : List (Html msg) -> Html msg
stackScopesDiv scopes =
    div [ class "cursor-scopes" ] scopes



noCursorView : Model s c -> Html (Msg s c)
noCursorView model =
    div [ class "cursor-view no-cursor-view" ]
        [ stackScopesDiv [ stackLevel <| text "No cursor" ]
        ]

hasCursorView : List ScopeMeta -> Model s c -> Html (Msg s c)
hasCursorView stack model =
    div [ class "cursor-view" ]
        [ List.map scopeStackLevel stack
                |> List.append [ scopeStackHead model.current ]
                |> List.intersperse (Html.span [class "cursor-scope-separator"] [ text " "])
                |> List.reverse
                |> stackScopesDiv
        ]


stackLevel : Html (Msg  s c) ->  Html (Msg s c)
stackLevel h =
     Html.span [class "cursor-scope"] [ h ]


scopeStackLevel : ScopeMeta -> Html (Msg s c)
scopeStackLevel meta =
    stackLevel <| text meta.displayName

scopeStackHead : ScopeMeta -> Html (Msg s c)
scopeStackHead meta =
    stackLevel <|
        Html.span [ class "head" ]
             [ text meta.displayName
             ]



-- SCOPE VIEW

