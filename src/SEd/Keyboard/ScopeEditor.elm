module SEd.Keyboard.ScopeEditor exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view, css



    , SExprScopeTraits
    )
{-| Describe me please...
-}

import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import SEd.Keyboard.KeyCommands exposing (..)
import Task


-- MODEL


{-| Traits
-}
type alias ScopeLikeTraits kind scope childKey data =
    { kindOf: scope -> kind
    , traitsFor: kind -> ScopeTraits kind scope childKey data
    }

type alias Model kind scope childKey data  =
    { data: scope
    , path: List childKey --(kind, childKey)
    , traits: ScopeLikeTraits kind scope childKey data
    }


modelFrom : ScopeLikeTraits k s i d -> s -> Model k s i d
modelFrom traits scope =
    { data = scope
    , path = []
    , traits = traits
    }


initialModel =
    modelFrom
        { kindOf = kindOf
        , traitsFor = traitsFor
        }
        (EList [ EKey "if", EList [EKey "=", EKey "i", EKey "len"]])

-- MSG


type Msg key
    = AddPath key
    | SetPath (List key)
    | Up
    | Down
    | Left
    | Right



-- SUBSCRIPTIONS


subscriptions : Model k s i d  -> Sub (Msg i)
subscriptions model =
    Sub.none



-- UPDATE


update : Msg i -> Model k s i d -> (Model k s i d, Cmd (Msg i))
update msg model =
    let orNothing = Maybe.withDefault (model, Cmd.none)
    in case msg of
        AddPath i ->
            {model | path = model.path ++ [i] } ! []

        SetPath is -> {model | path = is } ! []
        Up ->
             List.Extra.init model.path
                |> Maybe.map (\p -> { model | path = p } ! [])
                |> orNothing

        Down ->
            stepDown model |> orNothing

        Left -> updateStep .stepLeft model |> orNothing
        Right -> updateStep .stepRight model |> orNothing
--            updatePath
--                (\i s traits ->
--                    List.Extra.last i
--                        |> Maybe.andThen (\i -> traits.stepLeft i s)
--                        |> Maybe.map2 (\init ni -> init ++ [ni]) (List.Extra.init i)
--                ) model.path model
--                    |> orNothing

--        _ -> Nothing |> orNothing


{-| try to step into the current context
-}
stepDown : Model k s i d -> Maybe (Model k s i d, Cmd (Msg i))
stepDown model =
    scopeAndTraitsForPath model.path model
        |> Maybe.andThen (\(current, traits) -> traits.childKeys current)
        |> Maybe.andThen List.head
        |> Maybe.map (\head -> (model, Task.perform AddPath (Task.succeed head)))




{-| try to step into the current context
-}
updatePath
    : (List i -> s -> ScopeTraits k s i d -> Maybe (List i))
    -> List i -> Model k s i d -> Maybe (Model k s i d, Cmd (Msg i))
updatePath fn path model =
    let
        setPath : List i -> (Model k s i d, Cmd (Msg i))
        setPath newPath = (model, Task.perform SetPath (Task.succeed newPath))
    in
        List.Extra.init path
            |> Maybe.andThen (\path -> scopeAndTraitsForPath path model)
            |> Maybe.andThen (\(current, traits) ->
                Debug.log "NewPath:" <|
                    fn (Debug.log "oldPath:" path) current traits
                    )
            |> Maybe.map setPath


updateStep : (ScopeTraits k s i d -> i -> s -> Maybe i) ->
    Model k s i d -> Maybe (Model k s i d, Cmd (Msg i))
updateStep stepFn model =
  updatePath
        (\i s traits ->
            case List.reverse i of
                [] -> Nothing
                h :: ps ->
                        stepFn traits (Debug.log "head:" h) s
                            |> Debug.log "StepFnResult:"
                            |> Maybe.map (\ni -> List.reverse (ni :: ps) )


        ) model.path model





{-| get the current scope at the models path
-}
currentScope : Model k s i d -> Maybe s
currentScope model =
    sExprAt model.traits model.path model.data



scopeAndTraitsForPath : List i -> Model k s i d -> Maybe (s,ScopeTraits k s i d)
scopeAndTraitsForPath path model =
    let target = sExprAt model.traits path model.data
    in target
        |> Maybe.map model.traits.kindOf
        |> Maybe.map model.traits.traitsFor
        |> Maybe.map2 (,) target



sExprAt : ScopeLikeTraits k s i d -> List i -> s -> Maybe s
sExprAt scopeTraits path scope =
    case path of
        [] -> Just scope
        i :: path ->
            let
                kind = scopeTraits.kindOf scope
                traits = scopeTraits.traitsFor kind
            in
                traits.childScopeAt i scope
                    |> Maybe.andThen (sExprAt scopeTraits path)


-- VIEW: view



{-| view
-}
view : Model k s i d -> Html (Msg i)
view model =
    div [ class "scope-editor-view" ]
        [ text <| toString model
        , Html.hr [][]
        , infoRowBase "path" <| htmlList (infoRow "step" << toString) model.path

        , Html.button [ onClick Up ] [text "Up"]
        , Html.button [ onClick Down ] [text "Down"]

        , Html.button [ onClick Left ] [text "<- Left"]
        , Html.button [ onClick Right ] [text "Right -> "]

        , Html.hr [][]
        , sExprAt model.traits model.path model.data
            |> Maybe.map (sExpressionView model.traits)
            |> Maybe.withDefault (text "Cannot find SExpr at path")

        ]




{-| CSS parts for view
-}
viewCss : String
viewCss = """
.scope-editor-view {  }
.scope-editor-view .path { display: block; font-weight: bold; }
.scope-editor-view .path > ul  { list-style:none; display: inline-block; }
.scope-editor-view .path > ul > li { display: inline-block; }
.scope-editor-view .path:before { content: "path : " }
.scope-editor-view .path-step:before { content: "step : " }
"""





-- HELPERS

infoRowBase : String -> Html msg -> Html msg
infoRowBase k s = span [ class k, class "info-row" ] [ s ]

infoRow : String -> String -> Html msg
infoRow k s = infoRowBase k <| text s

htmlList fn es =
    Html.ul [] <|
        List.map (\e -> Html.li [] [ fn e ]) es




-- VIEW: sExpressionView



{-| s expression view
-}
sExpressionView : ScopeLikeTraits k s i d -> s -> Html (Msg i)
sExpressionView scopeTraits scope =
    let
        kind = scopeTraits.kindOf scope
        traits = scopeTraits.traitsFor kind
        childRange = traits.childKeys scope
     in
         div [ class "s-expr-view" ]
             [ infoRow "kind" <| toString kind
             , infoRow "base" <| toString traits.base
             , infoRow "data" <| toString scope
             , childRange
                 |> Maybe.map (htmlList (\i -> childKindAndDataView traits i scope))
                 |> Maybe.withDefault (text "")
             ]

{-| CSS parts for sExpressionView
-}
sExpressionViewCss : String
sExpressionViewCss = """
.s-expr-view { border: 1px solid; padding: 0.2em; }

.s-expr-view { font-family: "Fira Code", Monaco, Consolas, Courer New; font-size: 0.8em; whitespace: pre-wrap; }

.s-expr-view .info-row { display: block; }

.s-expr-view .kind { font-weight: bold;  }

.s-expr-view .kind:before { content: "kind .. " }
.s-expr-view .with:before { content: "with .. " }
.s-expr-view .data:before { content: "data .. " }
.s-expr-view .base:before { content: "base .. " }
"""





-- VIEW: childKindAndDataView



{-| child kind and data view
-}
childKindAndDataView : ScopeTraits k s i d -> i -> s -> Html (Msg i)
childKindAndDataView traits i scope =
    div [ class "child-kind-and-data-view" ]
        [ Html.button [ class "idx", onClick (AddPath i)] [ text <| toString i ]

        ,  traits.childDataAt i scope
            |> Maybe.map childDataView
            |> Maybe.withDefault (text "nada")

        , traits.childKindsAt i scope
            |> Maybe.map (htmlList (\(n,k) -> childKindView n k scope))
            |> Maybe.map (infoRowBase "opts")
            |> Maybe.withDefault (text "")

        ]

{-| CSS parts for childKindAndDataView
-}
childKindAndDataViewCss : String
childKindAndDataViewCss = """
.child-kind-and-data-view {  }
.child-kind-and-data-view .idx { cursor: pointer; text-decoration: underline; }
.child-kind-and-data-view .idx:before { content: "#"; }
.child-kind-and-data-view .opts:before { content: "opts : "; }
"""


-- VIEW: childDataKind


{-| child kind views
-}
childKindView : String -> k -> s -> Html (Msg i)
childKindView shortName scopeType scope =
    Html.li [ class "child-kind-views" ]
        [ infoRow "kind" <| toString scopeType
        , infoRow "short-name" shortName
        ]

{-| CSS parts for childKindViews
-}
childKindViewsCss : String
childKindViewsCss = """
.child-kind-views {  }
.child-kind-views .short-name:before { content: "from ~> " }
"""



-- VIEW: childDataView



{-| child data view
-}
childDataView : d -> Html (Msg i)
childDataView data =
    div [ class "child-data-view" ]
        [ infoRow "data" <| toString data
        ]

{-| CSS parts for childDataView
-}
childDataViewCss : String
childDataViewCss = """
.child-data-view {  }
"""



{-| test scope
-}
type Scope
    = EKey String
    | EList (List Scope)


type SExprScopeType
    = SKeyScope
    | SListScope

type alias SExprData = String
type alias SExprScopeTraits = ScopeTraits SExprScopeType Scope Int SExprData

----------



kindOf : Scope -> SExprScopeType
kindOf scope =
    case scope of
        EList _ -> SListScope
        EKey _ -> SKeyScope



traitsFor : SExprScopeType -> SExprScopeTraits
traitsFor s =
    case s of
        SKeyScope -> keyTraits
        SListScope -> listTraits






keyTraits : SExprScopeTraits
keyTraits =
    { leafScopeTraits
    | base = StringScope
--    , childKeys = always Nothing
--    , childKindsAt = keyChildKinds
    , childDataAt = keyChildData
--    , childScopeAt = keyChildScopeAt
--    , stepLeft = (\_ _ -> Nothing)
--    , stepRight = (\_ _ -> Nothing)
    }


--keyChildKinds : Int -> Scope -> Maybe (List (String, SExprScopeType))
--keyChildKinds i s = Nothing

{-| keyChildData
-}
keyChildData : Int -> Scope -> Maybe SExprData
keyChildData i s =
    case s of
        EKey s -> Just s
        _ -> Nothing

--
--{-| key Child Scope At
---}
--keyChildScopeAt : Int -> Scope -> Maybe Scope
--keyChildScopeAt i scope =
--    Nothing








listTraits : SExprScopeTraits
listTraits =
    { base = ListScope
    , childKeys = listChildKeys
    , childKindsAt = listChildKinds
    , childDataAt = listChildData
    , childScopeAt = listChildScopeFor

    , stepLeft = listStepLeft
    , stepRight = listStepRight
    }


listChildKinds : Int -> Scope -> Maybe (List (String, SExprScopeType))
listChildKinds i s =
    Just
        [ (":", SKeyScope)
        , ("(", SListScope)
        ]


{-| list Child Data
-}
listChildData : Int -> Scope -> Maybe SExprData
listChildData i s =
    case s of
        EList es ->
            List.Extra.getAt i es
                |> Maybe.map recursiveToData
        _ -> Nothing

{-| list Child Count
-}
listChildKeys : Scope -> Maybe (List Int)
listChildKeys s =
    case s of
        EList es -> Just <| List.range 0 (List.length es - 1)
        _ -> Nothing


{-| list Child Scope For
-}
listChildScopeFor : Int -> Scope -> Maybe Scope
listChildScopeFor i s =
    case s of
        EList es -> List.Extra.getAt i es
        _ -> Nothing


listStepLeft : Int -> Scope -> Maybe Int
listStepLeft i s =
    if i > 0 then Just <| i - 1  else Nothing

listStepRight : Int -> Scope -> Maybe Int
listStepRight i s =
    case s of
        EList es ->
            Debug.log "listStepRight" <|
                if (i + 1) < List.length (Debug.log "ES:" es) then Just (i + 1)  else Nothing
        _ -> Nothing




{-| recursive To Data
-}
recursiveToData : Scope -> SExprData
recursiveToData a =
    case a of
        EList es -> "(" ++ String.join " " (List.map recursiveToData es) ++ ")"
        EKey s -> ":" ++ s

-------------------------------------


type BasicScope
    = StringScope
    | IntScope
    | RecordScope
    | ListScope







type alias ScopeTraits scopeKey scope childKey data =
    { base: BasicScope
    , childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (List (String, scopeKey))
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , stepLeft: childKey -> scope -> Maybe childKey
    , stepRight: childKey -> scope -> Maybe childKey
    }


leafScopeTraits =
    { base = StringScope
    , childKeys = always Nothing
    , childKindsAt = (\_ _ -> Nothing)
    , childDataAt = (\_ _ -> Nothing)
    , childScopeAt = (\_ _ -> Nothing)

    , stepLeft = (\_ _ -> Nothing)
    , stepRight = (\_ _ -> Nothing)
    }


css : String
css = """
""" ++ viewCss ++ sExpressionViewCss ++ childDataViewCss ++ childKindViewsCss ++ childKindAndDataViewCss