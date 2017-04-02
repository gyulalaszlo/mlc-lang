module Codegen.Validator exposing (validateCode, Msg(..), Validity(..))
import Codegen.Lang exposing (..)
import Set exposing (Set)


-- ======================================

type Msg
    = Warning String
    | Error String

-- ======================================

type Validity
    = IsValid
    | HasWarnings (List Msg)
    | HasErrors (List Msg)

type alias ValidationResult =
    { warnings: List Msg
    , errors: List Msg
    }

-- ======================================

valid : ValidationResult
valid = ValidationResult [] []



-- ======================================


validateCode : Statements -> Validity
validateCode s =
    let
        checkStatement s errors =
            errors

        e = List.foldl checkStatement valid s
    in
        case (List.length e.errors, List.length e.warnings) of
            (0, 0) -> IsValid
            (0, _) -> HasWarnings e.warnings
            (_, _) -> HasErrors (List.append e.errors e.warnings)

-- ======================================


unusedVariables : Statement -> Set String -> Set String
unusedVariables s vars =
    case s of
        DeclareLocal t name ->
            Set.insert name vars
        _ -> vars


