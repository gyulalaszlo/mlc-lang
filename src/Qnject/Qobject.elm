module Qnject.Qobject exposing (..)
{-| Describe me please...
-}


import Json.Decode as Decode

type alias ClassName = String
type alias ObjectName = String
type alias Address = String


type QObjectKind
    = KindQWidget
    | KindQObject


type QAccessKind
    = PublicAccess
    | PrivateAccess
    | ProtectedAccess


type QMethodKind
    = Method
    | Constructor
    | Signal
    | Slot


type alias ObjectMethod =
    { name : String
    , kind: String
    , access: QAccessKind
    , signature: String
    }


{-| Represents a QObject gotten from the internal webserver
-}
type alias QObject =
    { objectName: ObjectName
    , address: Address
    , parentName: ObjectName
    , objectKind: QObjectKind
    , className: ClassName
    , superClass: ClassName
--    , methods: List ObjectMethod
    }

decodeQObjectKind : String -> Decode.Decoder QObjectKind
decodeQObjectKind kindStr =
    case kindStr of
        "widget" -> Decode.succeed KindQWidget
        _ -> Decode.succeed KindQObject


decodeQobject : Decode.Decoder QObject
decodeQobject =
  Decode.map6 QObject
    (Decode.field "objectName" Decode.string)
    (Decode.field "address" Decode.string)
    (Decode.field "parentName" Decode.string)

    (Decode.field "objectKind" Decode.string
        |> Decode.andThen decodeQObjectKind)

    (Decode.field "className" Decode.string)
    (Decode.field "superClass" Decode.string)





-- App



type alias QApp =
    { appName: String
    , address: Address
    , widgets: List QObject
    }


decodeQApp : Decode.Decoder QApp
decodeQApp =
    Decode.map3 QApp
--    Decode.map2 QApp
--    Decode.map2 (\a b -> QApp (Debug.log "A=" a) (Debug.log "B=" b))
        (Decode.field "appName" Decode.string)
        (Decode.field "qApp" Decode.string)
        (Decode.field "widgets" (Decode.list decodeQobject))
