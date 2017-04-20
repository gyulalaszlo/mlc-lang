module Qnject.QAppView exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| Describe me please...
-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Qnject.Qobject exposing (QApp, QObject)


-- MODEL


type alias Model =
    {
    }


initialModel : Model
initialModel =
    {
    }


type alias Context =
    { model: Model
    , app: QApp
    }

contextFrom : Model -> QApp -> Context
contextFrom = Context


-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []


-- VIEW


view : Context -> Html Msg
view ctx =
    div [ class "QAppView-view" ]
        [ headerView ctx
        , objectsView ctx.model ctx.app.widgets
        ]



-- VIEW: headerView



{-| header view
-}
headerView : Context -> Html Msg
headerView ctx =
    let { model, app } = ctx
    in div [ class "header-view" ]
        [ span [ class "app-name" ] [ text app.appName ]
        , text " "
        , span [ class "address" ] [ text app.address ]
        , text " "
        , span [ class "app-object-count" ]
            [ text " with "
            , Html.b [ class "object-count" ]
                [ text <| toString <| List.length <| app.widgets ]
            , text " widget objects"
            ]
        ]

{-| CSS parts for headerView
-}
headerViewCss : String
headerViewCss = """
.header-view {  }
.header-view .app-name { display:inline-block; }
.header-view .app-name:before { content: "Application: " }
"""



-- VIEW: objectsView



{-| generic object list widget
-}
objectsView : Model -> List QObject -> Html Msg
objectsView model objs =
    div [ class "objects-view" ]
        [ Html.table [ class "qobject-table table" ]
            [ Html.thead []
                [ 
                ]

            , Html.tbody [] <| List.map (qobjectTableRow model) objs
            ]
        ]

{-| CSS parts for objectsView
-}
objectsViewCss : String
objectsViewCss = """
.objects-view {  }
"""



-- VIEW: qobjectTableRow



{-| qobject table row
-}
qobjectTableRow : Model -> QObject -> Html Msg
qobjectTableRow  model obj =
    Html.tr
        [ class "qobject-table-row"
        , class <| "qobject-kind-" ++ toString obj.objectKind
        ]
        [ Html.td [ class "object-name" ] [ text obj.objectName ]
        , Html.td [ class "class-name" ] [ text obj.className ]
        , Html.td [ class "super-class" ] [ text obj.superClass ]
        , Html.td [ class "address" ] [ text obj.address ]
        ]

{-| CSS parts for qobjectTableRow
-}
qobjectTableRowCss : String
qobjectTableRowCss = """
.qobject-table-row {  }

"""








-- CSS

css : String
css = """
.QAppView-view {}
""" ++ headerViewCss ++ objectsViewCss ++ qobjectTableRowCss