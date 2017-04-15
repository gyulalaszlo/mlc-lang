module SEd.Scopes.SExprView exposing (view, css)
{-| Describe me please...
-}

import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick)
import List.Extra
import SEd.Scopes as Scopes exposing (BasicScope(ListScope), ScopeLikeTraits, ScopeTraits, scopeTraitsFor)
import SEd.Scopes.Model exposing (Model, currentScope, scopeAndTraitsForPath)
import SEd.Scopes.Msg exposing (Msg(..))

-- VIEW: sExpressionView

{-| s expression view
-}
view : ScopeLikeTraits k s i d -> s -> Html (Msg s i)
view scopeTraits scope =
    let
        kind =
            scopeTraits.kindOf scope

        traits =
            scopeTraits.traitsFor kind

        childRange =
            case traits.base of
                ListScope listTraits ->
                    listTraits.childKeys scope

                _ ->
                    Nothing
    in
        div [ class "s-expr-view" ]
            [ infoRow "kind" <| toString kind
            , infoRow "base" <| toString traits.base
            , traits.toData scope
                |> Maybe.map (infoRow "data" << toString)
                |> Maybe.withDefault (text "")
            , childRange
                |> Maybe.map (htmlList (\i -> childKindAndDataView traits i scope))
                |> Maybe.withDefault (text "")
            ]


{-| CSS parts for sExpressionView
-}
sExpressionViewCss : String
sExpressionViewCss =
    """
.s-expr-view { border: 1px solid; padding: 0.2em; }

.s-expr-view { font-family: "Fira Code", Monaco, Consolas, Courer New; font-size: 0.8em; whitespace: pre-wrap; }

.s-expr-view .info-row { display: block; }

.s-expr-view .kind { font-weight: bold;  }
.s-expr-view .data { font-size: 150%; padding: 1em; border:2px dotted;  }

.s-expr-view .kind:before { content: "kind .. " }
.s-expr-view .with:before { content: "with .. " }
/* .s-expr-view .data:before { content: "data .. " } */
.s-expr-view .base:before { content: "base .. " }
"""



-- VIEW: childKindAndDataView


{-| child kind and data view
-}
childKindAndDataView : ScopeTraits k s i d -> i -> s -> Html (Msg s i)
childKindAndDataView traits i scope =
    div [ class "child-kind-and-data-view" ] <|
        case traits.base of
            ListScope lTraits ->
                [ Html.button [ class "idx", onClick (AddPath i) ] [ text <| toString i ]
                , lTraits.childScopeAt i scope
                    |> Maybe.map childDataView
                    |> Maybe.withDefault (text "nada")
                    |> (\content -> div [ onClick (AddPath i) ] [ content ])
                , lTraits.childKindsAt i scope
                    |> Maybe.map (htmlList (\k -> childKindView k scope))
                    |> Maybe.map (infoRowBase "opts")
                    |> Maybe.withDefault (text "")
                ]

            _ ->
                []


{-| CSS parts for childKindAndDataView
-}
childKindAndDataViewCss : String
childKindAndDataViewCss =
    """
.child-kind-and-data-view {  }
.child-kind-and-data-view .data { cursor:pointer; }
.child-kind-and-data-view .data:hover { text-decoration: underline; }
.child-kind-and-data-view .idx { cursor: pointer; text-decoration: underline; }
.child-kind-and-data-view .idx:before { content: "#"; }
.child-kind-and-data-view .opts:before { content: "opts : "; }
"""



-- VIEW: childDataKind


{-| child kind views
-}
childKindView : k -> s -> Html (Msg s i)
childKindView scopeType scope =
    Html.li [ class "child-kind-views" ]
        [ infoRow "kind" <| toString scopeType
        ]


{-| CSS parts for childKindViews
-}
childKindViewsCss : String
childKindViewsCss =
    """
.child-kind-views {  }
.child-kind-views .short-name:before { content: "from ~> " }
"""



-- VIEW: childDataView


{-| child data view
-}
childDataView : d -> Html (Msg s i)
childDataView data =
    div [ class "child-data-view" ]
        [ infoRow "data" <| toString data
        ]


{-| CSS parts for childDataView
-}
childDataViewCss : String
childDataViewCss =
    """
.child-data-view {  }
"""




-- HELPERS


infoRowBase : String -> Html msg -> Html msg
infoRowBase k s =
    span [ class k, class "info-row" ] [ s ]


infoRow : String -> String -> Html msg
infoRow k s =
    infoRowBase k <| text s


htmlList fn es =
    Html.ul [] <|
        List.map (\e -> Html.li [] [ fn e ]) es





css : String
css =
    String.join "\n"
        [ sExpressionViewCss
        , childKindAndDataViewCss
        , childKindViewsCss
        , childDataViewCss
        ]
