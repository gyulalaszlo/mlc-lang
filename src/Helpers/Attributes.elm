{-

Attributes
-------

<Describe me if possible...>

-}
module Helpers.Attributes exposing (..)

-- import Html exposing (Html)

combinations : Int -> List a -> List (List a)
combinations n list =
    if n <= 0 then
        [ [] ]
    else
        case list of
            [] ->
                []

            x :: xs ->
                List.map ((::) x) (combinations (n - 1) xs) ++ combinations n xs


allCombinations : List a -> List (List a)
allCombinations list =
    case list of
        [] ->
            []

        [ x ] ->
            [ [ x ] ]

        _ ->
            List.concatMap
                (\n -> combinations n list)
            <|
                List.range 1 (List.length list)


classGen : List String -> (String -> String)
classGen prefixes =
    let
        prefixClass c =
            List.concatMap
                (\prefix ->
                    [ String.join "-" (prefix ++ [ c ])
                    , String.join "-" prefix
                    ]
                )
            <|
                allCombinations prefixes
    in
        \className ->
            String.join " " <| prefixClass className


