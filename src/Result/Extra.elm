module Result.Extra exposing
        ( andThen2

        , combine
        , andThenCombineWith


        )
{-| Describe me please...
|-}


{-| Applies f to the input results contents if its valid andThen combines the result with the source value into a tuple.

This is useful if the next operation in the chain needs both the original and the transformed value.
-}
andThenCombineWith : (a -> Result x b) -> Result x a -> Result x (a,b)
andThenCombineWith f a = Result.andThen (\aa -> Result.map (\bb -> (aa,bb)) (f aa)) a


{-| Like map2, but with a potentially failing operation
-}
andThen2 : (a -> b -> Result x c) -> Result x a -> Result x b -> Result x c
andThen2 f a b =
    Result.andThen (\aa -> Result.andThen (\bb -> f aa bb) b) a


{-| Combines two results into a tuple if both are Ok
-}
combine : Result x a -> Result x b -> Result x (a,b)
combine a b = Result.map2 (,) a b