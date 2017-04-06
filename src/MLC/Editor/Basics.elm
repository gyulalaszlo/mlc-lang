module MLC.Editor.Basics exposing (..)
{-| Describe me please...
-}

import MLC.Types exposing (..)

toDisplayString : Expression -> String
toDisplayString e =
    case e of
        EList [] -> "()"
        EList [EList (x :: _)] -> toDisplayString x
        EList [x] -> "(" ++ toDisplayString x ++ ")"
        EList (x :: _) -> "(" ++ toDisplayString x ++ " ...)"
        EKey s -> ":" ++ s
