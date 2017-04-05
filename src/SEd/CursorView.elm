module SEd.CursorView exposing
    ( Model
    , Traits

    , initialModel
    , setCursor

    , Msg(..)
    , subscriptions
    , update
    , view)
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
--import MLC.Cursor as Cursor exposing (Cursor)



-- MODEL


type alias Traits state cursor =
    { stateToString: state -> String
    , cursorToStringList: cursor -> List String
    }


type alias Model state cursor =
    { cursor: Maybe cursor
    , traits: Traits state cursor
    }


initialModel : Traits s c -> Model s c
initialModel traits =
    { cursor = Nothing
    , traits = traits
    }


setCursor : Maybe c -> Model s c -> Model s c
setCursor c model =
    { model | cursor = c }



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
    case model.cursor of
        Nothing -> noCursorView model
        Just cursor -> hasCursorView (model.traits.cursorToStringList cursor) model


cursorScopesDiv : List (Html msg) -> Html msg
cursorScopesDiv scopes =
    div [ class "cursor-scopes" ] scopes



noCursorView : Model s c -> Html (Msg s c)
noCursorView model =
    div [ class "cursor-view no-cursor-view" ]
        [ cursorScopesDiv [ cursorBit <| text "No cursor" ]
        ]

hasCursorView : List String -> Model s c -> Html (Msg s c)
hasCursorView cursorStr model =
    div [ class "cursor-view" ]
        [ List.map cursorScopeView cursorStr
            |> List.intersperse (Html.span [class "cursor-scope-separator"] [ text " "])
            |> cursorScopesDiv
        ]


cursorBit : Html (Msg  s c) ->  Html (Msg s c)
cursorBit h =
     Html.span [class "cursor-scope"] [ h ]


cursorScopeView : String -> Html (Msg s c)
cursorScopeView cursorStr =
    cursorBit <| text cursorStr


