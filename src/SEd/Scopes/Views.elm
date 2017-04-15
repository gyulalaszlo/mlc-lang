module SEd.Scopes.Views exposing (..)

{-| Describe me please...
-}

import Color
import Css.Background
import Css.Bits as Css exposing (borders)
import Css.Border
import Css.Font
import Css.Rectangle
import Css.Size exposing (em)
import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick)
import List.Extra
import SEd.Scopes as Scopes exposing (BasicScope(ListScope), ScopeLikeTraits, ScopeTraits, scopeTraitsFor)
import SEd.Scopes.Model exposing (Model, currentScope, scopeAndTraitsForPath)
import SEd.Scopes.Msg exposing (Msg(..))
import SEd.Scopes.SExprView


-- VIEW: toolbarView


{-| toolbar view
-}
toolbarView : Model k s i d -> Html (Msg s i)
toolbarView model =
    let
        scope =
            currentScope model

        orNothing =
            Maybe.withDefault (text "")

        pathListFoldStep localKey ( pathBuffer, pathBitsDone ) =
            let
                thisStepPath =
                    pathBuffer ++ [ localKey ]
            in
                ( thisStepPath, pathBitsDone ++ [ ( toString localKey, thisStepPath ) ] )

        pathList =
            Scopes.mapPath model.traits (\i ts s -> Ok <| ts.toLabel s) model.path model.data
                |> Result.withDefault []
                |> List.map2 (\path l -> ( l, path )) (List.Extra.inits model.path)
                |> List.append [ ( "root", [] ) ]
    in
        div [ class "toolbar-view" ]
            [ pathList
                |> List.map (\( i, p ) -> toolbarPathEntry i p)
                |> Html.nav [ class "path" ]
            , toolButtonsView model
            , specializedScopeEditors model.traits model.path model.data
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
toolbarPathEntry : String -> List i -> Html (Msg s i)
toolbarPathEntry i path =
    span
        [ class "toolbar-path-entry"
        , onClick <| SetPath path
        ]
        [ text i ]


{-| CSS parts for toolbarPathEntry
-}
toolbarPathEntryCss : String
toolbarPathEntryCss =
    """
.toolbar-path-entry { display: inline-block; line-height: 1.4em; padding: 0.3em 1em;  border-radius: 2em; cursor:pointer; }
.toolbar-path-entry:hover { text-decoration:underline; }
"""



-- VIEW: toolButtonsView


{-| tool buttons view
-}
toolButtonsView : Model k s i d -> Html (Msg s i)
toolButtonsView model =
    div [ class "tool-buttons-view" ]
        [ btn Up "Up"
        , btn Down "Down"
        , btn Left "<- Left"
        , btn Right "Right -> "
        , text " .. "
        , btn OpRemove "REMOVE !="
        ]


btn : msg -> String -> Html msg
btn msg label =
    Html.button
        [ class "toolbar-btn"
        , onClick msg
        ]
        [ text label ]


{-| CSS parts for toolButtonsView
-}
toolButtonsViewCss : String
toolButtonsViewCss =
    """
.tool-buttons-view .toolbar-btn {}

"""



-- SCOPE SPECIAL EDITORS -------------------------------------------------------

-- VIEW: specializedScopeEditors


{-| specialized scope editors
-}
specializedScopeEditors : ScopeLikeTraits k s i d -> Scopes.Path i -> s -> Html (Msg s i)
specializedScopeEditors traits path s =
    case Scopes.recursiveChildScopeAndTraitsAt traits path s of
        Err err ->
            div [ class "scope-error" ]
                [ text <| "Cannot find scope: " ++ Error.errorToString err ]

        Ok ( ts, s ) ->
            [ listScopeAppendEditorView, stringScopeEditorView ]
                |> List.map (\fn -> fn traits ts s)
                |> div [ class "specialized-scope-editors" ]


{-| CSS parts for specializedScopeEditors
-}
specializedScopeEditorsCss : String
specializedScopeEditorsCss =
    """
.specialized-scope-editors { padding: 1em;  }

.specialized-scope-editors,
.specialized-scope-editors input { font-size:19px;  }
"""
        ++ stringScopeEditorViewCss
        ++ listScopeAppendEditorViewCss



-- VIEW: stringScopeEditorView


{-| string scope editor view
-}
stringScopeEditorView : ScopeLikeTraits k s i d -> ScopeTraits k s i d -> s -> Html (Msg s i)
stringScopeEditorView globalTraits traits scope =
    case traits.base of
        Scopes.StringScope ts ->
            div [ class "string-scope-editor-view" ]
                [ Html.input
                    [ type_ "text", value <| ts.toString scope
                    , Html.Events.onInput <| OpSetString
                    ] []
                ]

        _ ->
            text ""


{-| CSS parts for stringScopeEditorView
-}
stringScopeEditorViewCss : String
stringScopeEditorViewCss =
    """
.string-scope-editor-view input {}
"""



-- VIEW: listScopeAppendEditorView


{-| Shows the possible append options for the currently selected scope
-}
listScopeAppendEditorView : ScopeLikeTraits k s i d -> ScopeTraits k s i d -> s -> Html (Msg s i)
listScopeAppendEditorView traits localTraits s =
    case Scopes.appendableTypes traits s of
        Err err ->
            text ""

        Ok ks ->
            ks
                |> List.map (\k -> appendButton traits (toString k) k)
                |> span []


{-| a button for appending a new scope to the end of the current
-}
appendButton : ScopeLikeTraits k s i d -> String -> k -> Html (Msg s i)
appendButton traits label k =
    Html.button
        [ onClick (OpAppend (traits.empty k)) ]
        [ text <| "Append " ++ label
        ]


{-| CSS parts for listScopeAppendEditorView
-}
listScopeAppendEditorViewCss : String
listScopeAppendEditorViewCss =
    """
.list-scope-append-editor-view {  }
"""


css : String
css =
    String.join "\n"
        [ toolbarViewCss
        , stringScopeEditorViewCss
        , toolButtonsViewCss
        , toolbarPathEntryCss
        , specializedScopeEditorsCss
        , SEd.Scopes.SExprView.css
        ]
