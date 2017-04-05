module SEd.UndoStack exposing
    ( Model
    , Traits

    , modelFromTraits
    , push, pop
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    )
{-| Describe me please...
-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import MLC.Types
import SEd.Operations as Operations exposing (Operation)


-- MODEL


type alias Model cursor node =
    { history: List (Operation cursor node)
    , traits: Traits cursor node
    }


{-| Creates a new Undo Stack model from the provided traits.
-}
modelFromTraits : Traits c n -> Model c n
modelFromTraits traits =
    { history = []
    , traits = traits
    }


-- STACK OPERATIONS


push : Operation c n -> Model c n -> Model c n
push op model =
    { model | history = op :: model.history }

pop : Model c n -> Model c n
pop model =
    case model.history of
        [] -> model
        h :: hs -> { model | history = hs }



-- TRAITS



{-| Describes how to convert from and the stacks node and cursor combination
-}
type alias Traits c n =
    { cursorToStringList: c -> List String
    , nodeToString: n -> String
    }


-- MSG


type Msg cursor node
    = Push (Operation cursor node)
    | Pop


-- SUBSCRIPTIONS


subscriptions : Model c n -> Sub (Msg c n)
subscriptions model =
    Sub.none



-- UPDATE


update : Msg c n -> Model c n -> (Model c n, Cmd (Msg c n))
update msg model =
    case msg of
        Push op -> push op model ! []
        Pop -> pop model ! []


-- VIEW

headerView : Model c n -> Html (Msg c n)
headerView model =
    Html.ul [class "undo-stack-header-view"]
        [ case model.history of
            [] ->
                Html.li [ class "empty-undo-stack" ] [ text "Nothing to undo" ]

            e :: _ ->
                stackEntry model.traits e
        ]



view : Model c n -> Html (Msg c n)
view model =
    div [ class "undo-stack-view" ]
        [ Html.ul [ class "undo-stack-list" ] <|
            List.map (stackEntry model.traits) model.history
        ]



stackEntry : Traits c n -> Operation c n -> Html (Msg c n)
stackEntry traits model =
    Html.li [ class "stack-entry" ]
        [ span [ class "stack-entry-header" ]
             [ entryToString traits model
             ]
        ]



entryToString : Traits c n -> Operation c n -> Html (Msg c n)
entryToString traits op =
    case op of
        Operations.InsertNodeAt c n -> insertNodeAt traits c n
        Operations.ReplaceNodeAt c n -> replaceNodeAt traits c n
        Operations.DeleteNodeAt c -> deleteNodeAt traits c



-- SHARED VIEWS

cursorView : Traits c n -> c -> Html msg
cursorView traits c =
    Html.span
        [ class "cursor" ]
        [ text <| String.join " / " <| traits.cursorToStringList c ]

nodeView : Traits c n -> n -> Html msg
nodeView traits n =
    Html.span
        [ class "node" ]
        [ text <|  traits.nodeToString n ]


-- CURSOR AND NODE OPS


opWithCursorAndNode : String -> String -> Traits c n -> c -> n -> Html (Msg c n)
opWithCursorAndNode kind label traits c n =
    span [ class "cursor-op" ]
         [ Html.label [ class "label", class (kind ++ "-label") ] [ text label ]
         , cursorView traits c
         , nodeView traits n
         ]



insertNodeAt : Traits c n -> c -> n -> Html (Msg c n)
insertNodeAt = opWithCursorAndNode "insert-node-at" "Insert"

replaceNodeAt : Traits c n -> c -> n -> Html (Msg c n)
replaceNodeAt = opWithCursorAndNode "replace-node-at" "Replace"



-- CURSOR ONLY OPERATIONS



opWithCursorOnly : String -> String -> Traits c n -> c -> Html (Msg c n)
opWithCursorOnly kind label traits c =
    span [ class "cursor-op" ]
         [ Html.label [ class "label", class (kind ++ "-label") ] [ text label ]
         , Html.b [] [ text <| toString c ]
         ]


deleteNodeAt : Traits c n -> c -> Html (Msg c n)
deleteNodeAt  = opWithCursorOnly "delete-node-at" "Delete Node"

