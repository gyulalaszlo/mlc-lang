module SEd.Keyboard.ScopeEditor exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view, css
    )
{-| Describe me please...
-}

import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import SEd.Keyboard.KeyCommands exposing (..)

-- MODEL


type alias Model =
    { data: Scope
    , path: List Int
    }


initialModel : Model
initialModel =
    { data = EList [ EKey "if", EList [EKey "=", EKey "i", EKey "len"]]
    , path = []
    }


-- MSG


type Msg
    = AddPath Int
    | Up



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddPath i ->
            {model | path = model.path ++ [i] } ! []

        Up ->
             List.Extra.init model.path
                |> Maybe.map (\p -> { model | path = p } ! [])
                |> Maybe.withDefault (model, Cmd.none)






sExprAt : List Int -> Scope -> Maybe Scope
sExprAt path scope =
    case path of
        [] -> Just scope
        i :: path ->
            let
                kind = kindOf scope
                traits = traitsFor kind
            in
                traits.childScopeAt i scope
                    |> Maybe.andThen (sExprAt path)


-- VIEW: view



{-| view
-}
view : Model -> Html Msg
view model =
    div [ class "scope-editor-view" ]
        [ text <| toString model
        , Html.hr [][]
        , infoRowBase "path" <| htmlList (infoRow "step" << toString) model.path
        , Html.button [ onClick Up ] [text "Up"]
        , Html.hr [][]
        , sExprAt model.path model.data
            |> Maybe.map sExpressionView
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
sExpressionView : Scope -> Html Msg
sExpressionView scope =
    let
        kind = kindOf scope
        traits = traitsFor kind
        childRange = traits.childCount scope
             |> Maybe.map (\len -> List.range 0 (len - 1))
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
childKindAndDataView : SExprScopeTraits -> Int -> Scope -> Html Msg
childKindAndDataView traits i scope =
    div [ class "child-kind-and-data-view" ]
        [ Html.button [ class "idx", onClick (AddPath i)] [ text <| toString i ]

        , childDataView <| traits.childDataAt i scope

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
childKindView : String -> SExprScopeType -> Scope -> Html Msg
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
childDataView : Maybe SExprData -> Html Msg
childDataView data =
    div [ class "child-data-view" ]
        [ infoRow "data" <|
            case data of
                Just data -> data
                Nothing -> "~~~ NOTHING ~~~"
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
type alias SExprScopeTraits = ScopeTraits SExprScopeType Scope SExprData

----------



kindOf : Scope -> SExprScopeType
kindOf scope =
    case scope of
        EList _ -> SListScope
        EKey _ -> SKeyScope



traitsFor : SExprScopeType -> ScopeTraits SExprScopeType Scope String
traitsFor s =
    case s of
        SKeyScope -> keyTraits
        SListScope -> listTraits






keyTraits : ScopeTraits SExprScopeType Scope String
keyTraits =
    { base = StringScope
    , childCount = always Nothing
    , childKindsAt = keyChildKinds
    , childDataAt = keyChildData
    , childScopeAt = keyChildScopeAt
    }


keyChildKinds : Int -> Scope -> Maybe (List (String, SExprScopeType))
keyChildKinds i s = Nothing

{-| keyChildData
-}
keyChildData : Int -> Scope -> Maybe SExprData
keyChildData i s =
    case s of
        EKey s -> Just s
        _ -> Nothing


{-| key Child Scope At
-}
keyChildScopeAt : Int -> Scope -> Maybe Scope
keyChildScopeAt i scope =
    Nothing








listTraits : ScopeTraits SExprScopeType Scope String
listTraits =
    { base = ListScope
    , childCount = listChildCount
    , childKindsAt = listChildKinds
    , childDataAt = listChildData
    , childScopeAt = listChildScopeFor
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
listChildCount : Scope -> Maybe Int
listChildCount s =
    case s of
        EList es -> Just <| List.length es
        _ -> Nothing


{-| list Child Scope For
-}
listChildScopeFor : Int -> Scope -> Maybe Scope
listChildScopeFor i s =
    case s of
        EList es -> List.Extra.getAt i es
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







type alias ChildKinds msg scope =
    Int -> scope -> Maybe (List (String, msg))

type alias ChildData scope data =
    Int -> scope -> Maybe data


type alias ScopeTraits msg scope data =
    { base: BasicScope
    , childCount: (scope -> Maybe Int)
    , childKindsAt: ChildKinds msg scope
    , childDataAt: ChildData scope data
    , childScopeAt: Int -> scope -> Maybe Scope
    }





css : String
css = """
""" ++ viewCss ++ sExpressionViewCss ++ childDataViewCss ++ childKindViewsCss ++ childKindAndDataViewCss