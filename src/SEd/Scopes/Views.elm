module SEd.Scopes.Views exposing (..)

{-| Describe me please...
-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import SEd.Scopes exposing (BasicOperation, ScopeTraits, allOperations)
import SEd.Scopes.Model exposing (Model, ScopeLikeTraits, currentScope, scopeAndTraitsForPath)
import SEd.Scopes.Msg exposing (Msg(..))


-- VIEW: toolbarView


{-| toolbar view
-}
toolbarView : Model k s i d -> Html (Msg i)
toolbarView model =
    let
        scope =
            currentScope model
    in
        div [ class "toolbar-view" ]
            [ div [ class "path" ]
                [ span [ class "toolbar-path-entry" ] [ text "root" ]
                , htmlList (\( i, p ) -> toolbarPathEntry i p) <|
                    Tuple.second <|
                        List.foldl
                            (\i ( pp, ps ) ->
                                let
                                    p =
                                        pp ++ [ i ]
                                in
                                    ( p, ps ++ [ ( i, p ) ] )
                            )
                            ( [], [] )
                            model.path
                ]
            , Html.button [ onClick Up ] [ text "Up" ]
            , Html.button [ onClick Down ] [ text "Down" ]
            , Html.button [ onClick Left ] [ text "<- Left" ]
            , Html.button [ onClick Right ] [ text "Right -> " ]
            , scopeAndTraitsForPath model.path model
                |> Maybe.map
                    (\( c, t ) ->
                        List.filterMap
                            (\op ->
                                t.operationSupports op c
                                    |> Maybe.map (\s -> ( op, s ))
                            )
                            allOperations
                    )
                |> Maybe.map (htmlList (\( op, s ) -> operationsView op s))
                |> Maybe.withDefault (text "No supported operations")
            ]


{-| CSS parts for toolbarView
-}
toolbarViewCss : String
toolbarViewCss =
    """
.toolbar-view  { font-size: 0.8em; }
.toolbar-view  .path { font-family: "Fira Code", Consolas, Courier New; }

.toolbar-view .cursor-scope-separator { border-right: 3px solid; border-radius: 2em; color: #555; }

.toolbar-view .cursor-scope-nth,
.toolbar-view .cursor-scope-leaf {  }

.scope-editor-view .path { display: block; font-weight: bold; }
.scope-editor-view .path > ul  { list-style:none; display: inline-block; padding:0; margin:0;  }
.scope-editor-view .path > ul > li { display: inline-block; }
.scope-editor-view .path:before { content: "path : " }
.scope-editor-view .path-step:before { content: "step : " }
"""



-- VIEW: toolbarPathEntry


{-| toolbar path entry
-}
toolbarPathEntry : i -> List i -> Html (Msg i)
toolbarPathEntry i path =
    div [ class "toolbar-path-entry" ]
        [ infoRowBase "step" <| span [ onClick <| SetPath path ] [ text <| toString i ]
        ]


{-| CSS parts for toolbarPathEntry
-}
toolbarPathEntryCss : String
toolbarPathEntryCss =
    """
.toolbar-path-entry { display: inline-block; line-height: 1.4em; padding: 0.3em 1em;  border-radius: 2em; border-right: 4px solid; }
.toolbar-path-entry:before { content: " -> "; }
"""



-- VIEW: operationsView


{-| operations view
-}
operationsView : BasicOperation -> List k -> Html (Msg i)
operationsView op scopes =
    div [ class "operations-view" ]
        [ span [ class "operation-name" ] [ text <| toString op ]
        , Html.ul [] <|
            List.map (\scopeType -> Html.li [] [ text <| toString scopeType ]) scopes
        ]


{-| CSS parts for operationsView
-}
operationsViewCss : String
operationsViewCss =
    """
.operations-view {  }
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



-- VIEW: sExpressionView


{-| s expression view
-}
sExpressionView : ScopeLikeTraits k s i d -> s -> Html (Msg i)
sExpressionView scopeTraits scope =
    let
        kind =
            scopeTraits.kindOf scope

        traits =
            scopeTraits.traitsFor kind

        childRange =
            traits.childKeys scope
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
        [ Html.button [ class "idx", onClick (AddPath i) ] [ text <| toString i ]
        , traits.childDataAt i scope
            |> Maybe.map childDataView
            |> Maybe.withDefault (text "nada")
        , traits.childKindsAt i scope
            |> Maybe.map (htmlList (\k -> childKindView k scope))
            |> Maybe.map (infoRowBase "opts")
            |> Maybe.withDefault (text "")
        ]


{-| CSS parts for childKindAndDataView
-}
childKindAndDataViewCss : String
childKindAndDataViewCss =
    """
.child-kind-and-data-view {  }
.child-kind-and-data-view .idx { cursor: pointer; text-decoration: underline; }
.child-kind-and-data-view .idx:before { content: "#"; }
.child-kind-and-data-view .opts:before { content: "opts : "; }
"""



-- VIEW: childDataKind


{-| child kind views
-}
childKindView : k -> s -> Html (Msg i)
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
childDataView : d -> Html (Msg i)
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


css : String
css =
    String.join "\n"
        [ toolbarViewCss
        , toolbarPathEntryCss
        , operationsViewCss
        , sExpressionViewCss
        , childDataViewCss
        , childKindViewsCss
        , childKindAndDataViewCss
        ]