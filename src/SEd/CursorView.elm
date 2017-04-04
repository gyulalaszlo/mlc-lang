module SEd.CursorView exposing
    (Model

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
import MLC.Cursor as Cursor exposing (Cursor)



-- MODEL


type alias Model k =
    { cursor: Cursor k
    }


initialModel : Model k
initialModel =
    { cursor = Cursor.leaf
    }


setCursor : Cursor k -> Model k -> Model k
setCursor c model =
    { model | cursor = c }

-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model k -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model k -> (Model k, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []


-- VIEW


view : Model k -> Html Msg
view model =
    div [ class "cursor-view" ]
        [ cursorScopeView model.cursor
            |> List.intersperse (Html.span [class "cursor-scope-separator"] [ text " "])
            |> div [ class "cursor-scopes" ]
        ]


cursorBit : Html Msg -> List (Html Msg) -> List (Html Msg)
cursorBit h els =
     (Html.span [class "cursor-scope"] [ h ] ) :: els


cursorScopeView : Cursor k -> List (Html Msg)
cursorScopeView cursor =
     case cursor of
        Cursor.Leaf ->
            cursorBit
                (Html.span
                    [ class "cursor-scope-leaf"]
                    [ text "..."])
                []

        Cursor.Nth k cc ->
            cursorBit
                (Html.span
                    [class "cursor-scope-nth"]
                    [ text "nth: #"
                    , text <| toString <| k
                    ])
                (cursorScopeView cc)
