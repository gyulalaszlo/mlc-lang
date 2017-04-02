module Codegen.Sample exposing (..)

import Codegen.Lang exposing (..)
import Codegen.Codegen exposing (toCCode)
import Codegen.Validator as Validator exposing (Validity(..), validateCode)

import Html exposing (text, Html)
import Html.Attributes exposing (style)


type alias Model =
    { code : List Statement }

--loop : List (Identifier, Expression) -> Statement
--loop

model : Model
model =
    { code =
        [ DeclareLocal
            (Integral "size_t") "i"
        , DeclareLocal
            (Const (Integral "size_t")) "length"
        , DeclareLocal
            (Pointer (Integral "char")) "input"

        -- x = x + y + 1
        , ExpressionStatement
            (BinaryExpression
                OpAssign
                (ValueOf "x")
                (BinaryExpression
                    (OpIntPlus)
                    (IntegerLiteral "12")
                    (BinaryExpression
                        (OpIntMinus)
                        (IntegerLiteral "14")
                        (IntegerLiteral "12"))))

        , ForLoop
            (BinaryExpression OpAssign (ValueOf "i") (IntegerLiteral "0") )
            (BinaryExpression OpCompLt (ValueOf "i") (ValueOf "length"))
            (BinaryExpression OpAssign (ValueOf "i")
                (BinaryExpression OpIntPlus (ValueOf "i") (IntegerLiteral "1")))

            [ IfStatement
                (BinaryExpression
                    OpCompGt
                    (ValueOf "i")
                    (IntegerLiteral "5"))
                [ (ExpressionStatement
                    (BinaryExpression
                        OpAssign
                            (ValueOf "i")
                            (IntegerLiteral "0")))
                ]
            ]

        , Return (ValueOf "i")
        ]
    }


compile : List Statement -> Result (List Validator.Msg) String
compile s =
    case validateCode s of

        IsValid -> Ok (String.join "\n" (toCCode s))
        HasWarnings e -> Err e
        HasErrors e -> Err e

warningView : Validator.Msg -> Html msg
warningView w =
    case w of
        Validator.Warning s -> Html.li [] [ text s ]
        Validator.Error s -> Html.li [] [ text s ]

codeView : String -> Html msg
codeView c =
    Html.pre
        [ style
            [ ( "padding", "10px" )
            , ( "margin", "0" )
            , ( "font-size", "18px" )
            , ( "height", "95%")
            , ( "background", "#222")
            , ( "color", "#999" )
            ]
        ]
        [ Html.code [] [ text c ]
        ]
