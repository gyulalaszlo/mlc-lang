module SEd.UndoStack exposing
    ( Model
    , initialModel
    , push, pop
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    )
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import SEd.Operations exposing (Operation)


-- MODEL


type alias Model cursor node =
    { history: List (Operation cursor node)
    }


initialModel : Model cursor node
initialModel =
    { history = []
    }


push : Operation c n -> Model c n -> Model c n
push op model =
    { model | history = op :: model.history }

pop : Model c n -> Model c n
pop model =
    case model.history of
        [] -> model
        h :: hs -> { model | history = hs }


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


view : Model c n -> Html (Msg c n)
view model =
    div [ class "undo-stack-view" ] <|
        List.map stackEntry model.history



stackEntry : Operation c n -> Html (Msg c n)
stackEntry model =
    div [ class "stack-entry" ]
        [ text <| toString model
        ]
