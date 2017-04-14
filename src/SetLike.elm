module SetLike exposing (..)

{-| Describe me please...
-}

import Dict exposing (Dict)
import Set exposing (Set)


type alias SetLike k v =
    { set : Set k
    , values : Dict k v
    , vToK : v -> k
    }


emptySetLike : (v -> k) -> SetLike k v
emptySetLike vToK =
    SetLike (Set.empty) (Dict.empty) vToK


insert : v -> SetLike k v -> SetLike k v
insert v s =
    let
        k =
            s.vToK v
    in
        if Set.member k s.set then
            s
        else
            { s
                | set = Set.insert k s.set
                , values = Dict.insert k v s.values
            }


member : v -> SetLike k v -> Bool
member v s =
    let
        k =
            s.vToK v
    in
        Set.member k s.set
