module AstView.Main exposing (..)
{-| Describe me please...
|-}

import CAsm exposing (CAsm)
import CAsm.Error as Error exposing (Error, errorToString)
import CAst exposing (StatementList)
import CAst.AstBuilder exposing (toAst)
import CAst.AstPrinter exposing (Token, statementListToTokens)
import CAst.CodeStyle exposing (applyCodeStyle, defaultCodeStyle)
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (attribute, class)
import Task

type alias Code =
    { assembly: CAsm
    , ast: Result Error StatementList
    }



-- MODEL


type alias Model =
    { code: Maybe Code
    , tokens: List Token
    }

initialModel : Model
initialModel =
    { code = Nothing
    , tokens = []
    }


-- MESSAGES


type Msg
    = SetAssembly CAsm
--    | OnCompileDone (Result Error StatementList)



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetAssembly a ->
            {model
                | code = Just { assembly = a
                         , ast = toAst a }
                } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    case model.code of
        Nothing ->
            div
                [class "error"]
                [ text "No code yet"
                ]

        Just {assembly, ast} ->
            div [class "ast-view"]
                [ astView ast
                ]

tokenList : List Token -> List (Html Msg)
tokenList ts =
    List.map
        (\{class, text, tag} -> Html.span
            [Html.Attributes.class ("token token-" ++ class ++ " " ++ class)
            , attribute "data-tag" tag
            , attribute "data-class" class
            ]
            [Html.text text] )
        ts

astView : Result Error StatementList -> Html Msg
astView s =
    case s of
        Ok ast ->
            Html.code []
                [ Html.pre [class "c-code"]
                    <| tokenList
                    <| applyCodeStyle defaultCodeStyle
                    <| statementListToTokens ast
                ]
        Err errors -> Html.pre [] [ Html.text <| errorToString errors]





--main =
--    Html.div []
--        [ css
--        , astView sample
--        , Html.hr [] []
--        , Html.pre [] [ Html.text <| prettyPrint sample ]
--        ]
