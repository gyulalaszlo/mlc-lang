module SEd.Scopes.Msg exposing (..)
{-| Messages for the scope editor
-}

-- MSG


{-| Basic commands to be issued to the scope editor / navigator.
key is the "node index" type (usually int).
-}
type Msg key
    = AddPath key
    | SetPath (List key)
    | Up
    | Down
    | Left
    | Right

    | OpRemove


