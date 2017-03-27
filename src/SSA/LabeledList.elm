{-

   Block
   -------

   <Describe me if possible...>

-}


module SSA.LabeledList exposing (..)

--import Dict exposing (Dict)
-- import Html exposing (Html)
{-
   The symbol table
-}


type alias LabeledList label nodes =
    Maybe ( label, List nodes )


label : LabeledList l n -> Maybe l
label l =
    Maybe.map Tuple.first l



{-
   Returns the values of the list or an empty array if
   its empty
-}


values : LabeledList l n -> List n
values l =
    Maybe.map Tuple.second l
        |> Maybe.withDefault []


mapBoth : (l -> a) -> (n -> b) -> LabeledList l n -> LabeledList a b
mapBoth fl fn lst =
    Maybe.map (\( l, n ) -> ( fl l, List.map fn n )) lst


mapLabel : (l -> b) -> LabeledList l n -> LabeledList b n
mapLabel f lst =
    Maybe.map (\( l, n ) -> ( f l, n )) lst


mapValues : (n -> b) -> LabeledList l n -> LabeledList l b
mapValues f =
    mapBoth identity f


{-

    Creates a new labeledlist from a regular list by using f
    to create a label from the head element.

-}
from : (n -> l) -> List n -> LabeledList l n
from f ns =
    case ns of
        [] -> Nothing
        x :: _ -> Just (f x, ns)


{-
   Concatenates two chains of blocks.
   The new start label will be the label of the first chain.

   Does not care about chaining blocks together
-}


concat : List (LabeledList l n) -> LabeledList l n
concat l =
    let
        f b a =
            case ( a, b ) of
                ( Nothing, Nothing ) ->
                    empty

                ( Just _, Nothing ) ->
                    a

                ( Nothing, Just _ ) ->
                    b

                ( Just ( al, a ), Just ( _, b ) ) ->
                    Just ( al, List.concat [ a, b ] )

    in
        List.foldl f empty l


{-
   An empty list of blocks
-}


empty : LabeledList l n
empty =
    Nothing
