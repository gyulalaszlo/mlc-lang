module Qnject.Viewers.QAppView exposing
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

import Effects exposing (Effects)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Qnject.Qobject exposing (Address, QApp, QObject)
import Qnject.Connection as Connection
import Qnject.ViewerEffects exposing (ViewerEffect(OpenObjectView))



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
    , connection: Connection.Model
    }

contextFrom : Model -> Connection.Model -> Context
contextFrom = Context


-- MSG


type Msg
    = OpenAddress Address



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg, Effects ViewerEffect)
update msg model =
    case msg of
        OpenAddress addr -> (model, Cmd.none, Effects.from <| OpenObjectView addr)


-- VIEW


view : Context -> Html Msg
view {model, connection} =
    case connection.app of
        Nothing -> text "No app"
        Just app ->
            div [ class "QAppView-view" ]
                [ headerView model app
                , objectsView model app.widgets
                ]



-- VIEW: headerView



{-| header view
-}
headerView : Model -> QApp -> Html Msg
headerView model app =
    div [ class "header-view" ]
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
.qobject-table { width:100%;  }
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
        [ Html.td [ class "address", onClick <| OpenAddress obj.address ] [ text obj.address ]
        , Html.td [ class "object-name" ] [ text obj.objectName ]
        , Html.td [ class "class-name"] [ text obj.className ]
        , Html.td [ class "super-class" ] [ text obj.superClass ]
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