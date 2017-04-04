module CAsm.Error exposing (..)
{-| Describe me please...
|-}
import Indented exposing (Line(..), applyIndents, indentWith)

type Error
    = Error { msg : String , children: List Error }

empty : Error
empty = Error { msg = "", children = [] }

make : String -> Error
make s = Error { msg = s, children = [] }

makeMsg : List String -> Error
makeMsg s = Error { msg = (String.join " " s), children = [] }

wrapIn : String -> List Error -> Error
wrapIn s es = Error { msg = s, children = es }


wrapError : String -> Result Error a -> Result Error a
wrapError s r =
    Result.mapError (\e -> wrapIn s [e]) r


wrapErrorMsg : List String -> Result Error a -> Result Error a
wrapErrorMsg ss r =
    wrapError (String.join " " ss) r

{-| Converts a list of potential errors to a
-}
wrapErrors : String -> List (Result Error a) -> Result Error (List a)
wrapErrors s rs =
    let
        folder r m =
            case (r, m) of
                (Ok oo, Ok mo) -> Ok (oo :: mo)
                (Ok oo, Err mo) -> m
                (Err oo, Ok mo) -> Err [oo]
                (Err oo, Err mo) -> Err (oo :: mo)

    in
        List.foldr folder (Ok []) rs
            |> Result.mapError (wrapIn s)


errorToString : Error -> String
errorToString e =
    toStringHelper e
        |> indentWith "  "

toStringHelper e =
    case e of
        Error { msg, children } ->
            List.concat
                [ [Text msg, Indent]
                , List.concatMap toStringHelper children
                , [Outdent]
                ]