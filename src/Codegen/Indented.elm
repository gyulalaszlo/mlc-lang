module Codegen.Indented exposing (Line(..), applyIndents, concatLine, concatStatement)


type Line
    = Text String
    | Indent
    | Outdent


mapJoin : String -> (a -> String) -> List a -> String
mapJoin joiner f lst =
    List.map f lst |> String.join joiner



{-
   Creates a list of lines from individual tokens
-}


concatLine : List String -> List Line
concatLine bits =
    [ Text (String.join " " bits) ]


concatStatement : List String -> List Line
concatStatement bits =
    [ Text ((String.join " " bits) ++ ";") ]


type alias AppendLineIndentState =
    { indent : Int, lines : List String }


applyLineIndent : Line -> AppendLineIndentState -> AppendLineIndentState
applyLineIndent line state =
    let
        withNewLine s =
            state.lines ++ [ (String.repeat state.indent "\t") ++ s ]
    in
        case line of
            Text s ->
                { state | lines = withNewLine s }

            Indent ->
                { state | indent = state.indent + 1 }

            Outdent ->
                { state | indent = max (state.indent - 1) 0 }



{-
   Applies the indentation guides to the text
-}


applyIndents : List Line -> List String
applyIndents lines =
    let
        init =
            { lines = [], indent = 0 }

        indentedLines =
            List.foldl applyLineIndent init lines
    in
        indentedLines.lines
