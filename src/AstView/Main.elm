module AstView.Main exposing (..)
{-| Describe me please...
|-}

import AstView.CodeStyleEditor as CodeStyleEditor
import AstView.AsmView as AsmView
import CAsm exposing (CAsm)
import CAsm.Error as Error exposing (Error, errorToString)
import CAst exposing (StatementList)
import CAst.AstBuilder exposing (toAst)
import CAst.AstPrinter exposing (Token, statementListToTokens)
import CAst.CodeStyle exposing (CodeStyle, applyCodeStyle, defaultCodeStyle)
import Helpers.SplitLayout as SplitLayout
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Task

type alias Code =
    { assembly: CAsm
    , ast: Result Error StatementList
    }



-- MODEL


type alias Model =
    { code: Maybe Code
    , tokens: List Token
    , codeStyleEditor: CodeStyleEditor.Model
    , codeStyleEditorShown: Bool

    , asmView: AsmView.Model
    , asmViewShown: Bool

    , splitLayout: SplitLayout.Model
    }

initialModel : Model
initialModel =
    { code = Nothing
    , tokens = []
    , codeStyleEditor = CodeStyleEditor.initialModel
    , codeStyleEditorShown = False

    , asmView = AsmView.initialModel
    , asmViewShown = False

    , splitLayout = SplitLayout.initialModel
    }


-- MESSAGES


type Msg
    = SetAssembly CAsm
    | CodeStyleEditorMsg CodeStyleEditor.Msg

    | SetCodeStyleEditorShown Bool
    | SelectTokenTag String

    | AsmViewMsg AsmView.Msg
    | SetAsmViewShown Bool

    | SplitLayoutMsg SplitLayout.Msg
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

        CodeStyleEditorMsg m ->
            let
                (sm, sc) = CodeStyleEditor.update m model.codeStyleEditor
            in
                ({ model | codeStyleEditor = sm }, Cmd.map CodeStyleEditorMsg sc)

        SetCodeStyleEditorShown s ->
            { model | codeStyleEditorShown = s } ! []

        SelectTokenTag s ->
            let
                (sm, sc) = CodeStyleEditor.update (CodeStyleEditor.Select s) model.codeStyleEditor
            in
                ({ model
                    | codeStyleEditor = sm
                    , codeStyleEditorShown = (s /= "")
                    }
                , Cmd.map CodeStyleEditorMsg sc)

        AsmViewMsg m ->
            let
                (sm, sc) = AsmView.update m model.asmView
            in
                ({ model | asmView = sm }, Cmd.map AsmViewMsg sc)

        SetAsmViewShown s ->
            { model | asmViewShown = s } ! []

        SplitLayoutMsg m ->
            let
                (sm, sc) = SplitLayout.update m model.splitLayout
            in
                ({ model | splitLayout = sm }, Cmd.map SplitLayoutMsg sc)






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    div [class "ast-view"]
        [ headerView model
        , codeView model
        ]

codeView : Model -> Html Msg
codeView model =
    case model.code of
        Nothing ->
            div
                [class "error"]
                [ text "No code yet"
                ]

        Just {assembly, ast} ->
            div [class "code-view"]
                [ SplitLayout.view model.splitLayout <|
                    List.concat
                        [ if model.asmViewShown
                                then
                                    [Html.map AsmViewMsg <|
                                        AsmView.view model.asmView assembly]
                                else
                                    []
                        , if model.codeStyleEditorShown
                            then
                                [Html.map CodeStyleEditorMsg <|
                                    CodeStyleEditor.view model.codeStyleEditor]
                            else
                                []
                        , [astView model.codeStyleEditor.codeStyle ast]
                        ]
                ]



tokenList : List Token -> List (Html Msg)
tokenList ts =
    List.map
        (\{class, text, tag} -> Html.span
            [Html.Attributes.class ("token token-" ++ class ++ " " ++ class)
            , attribute "data-tag" tag
            , attribute "data-class" class
            , onClick (SelectTokenTag tag)
            ]
            [Html.text text] )
        ts

astView : CodeStyle -> Result Error StatementList -> Html Msg
astView codeStyle s =
    case s of
        Ok ast ->
            Html.code []
                [ Html.pre [class "c-code"]
                    <| tokenList
                    <| applyCodeStyle codeStyle
                    <| statementListToTokens ast
                ]
        Err errors -> Html.pre [] [ Html.text <| errorToString errors]



-- HEADER VIEW


headerView : Model -> Html Msg
headerView model =
    div [class "ast-view-header"]
        [ directionButton model.splitLayout.direction
        , showButton "Code Style" SetCodeStyleEditorShown model.codeStyleEditorShown
        , showButton "Assembly" SetAsmViewShown model.asmViewShown
        ]



showButton : String -> (Bool -> msg) -> Bool -> Html msg
showButton label msg v =
    Html.button
        [ onClick <| msg (not v), class <| if v then "on" else "off" ]
        [ text <| label
        ]

directionButton : SplitLayout.Direction -> Html Msg
directionButton d =
    case d of
        SplitLayout.Horizontal ->
            Html.button
                [ onClick <| SplitLayoutMsg <| SplitLayout.SetDirection SplitLayout.Vertical]
                [ text "Split ||"
                ]
        SplitLayout.Vertical ->
            Html.button
                [ onClick <| SplitLayoutMsg <| SplitLayout.SetDirection SplitLayout.Horizontal]
                [ text "Split =="
                ]




--main =
--    Html.div []
--        [ css
--        , astView sample
--        , Html.hr [] []
--        , Html.pre [] [ Html.text <| prettyPrint sample ]
--        ]
