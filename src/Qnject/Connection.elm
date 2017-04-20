module Qnject.Connection exposing (..)
{-| Describe me please...
-}

type alias Connection =
    { url: String
    }


default : Connection
default =
    { url = "http://localhost:8000"
    }


url : String -> Connection -> String
url path {url} =
    url ++ "/api" ++ path