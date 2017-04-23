module Qnject.Viewers.QObjectOverview
    exposing
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

import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, src, type_, value)
import Qnject.Connection as Connection
import Qnject.Qobject exposing (Address, ObjectMethod, QObjectDetails, QObjectSummary)


-- MODEL


type alias Model =
    { address : Address
    }


initialModel : Address -> Model
initialModel address =
    { address = address
    }


type alias Context =
    { model : Model
    , connection : Connection.Model
    }



-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []



-- VIEW


view : Context -> Html Msg
view ctx =
    let
        obj =
            Connection.objectAt ctx.model.address ctx.connection
    in
        div [ class "qobject-view" ]
            [ screenShotView ctx
            , Maybe.map baseDataView obj |> Maybe.withDefault (text <| "No data for object: " ++ ctx.model.address)
            ]



-- CSS


css : String
css =
    """
.qobject-view {}
""" ++ screenshotViewCss ++ baseDataViewCss



-- VIEW: screenshotView


{-| screenshot view
-}
screenShotView : Context -> Html Msg
screenShotView { connection, model } =
    let
        address =
            model.address

        srcUrl =
            Connection.url ("/qwidgets/by-address/grab/" ++ address) connection
    in
        div [ class "screenshot-view" ]
            [ Html.img [ class "screenshot", src srcUrl ] []
            ]


{-| CSS parts for screenshotView
-}
screenshotViewCss : String
screenshotViewCss =
    """
.screenshot-view {  }
.screenshot-view img.screenshot { max-width:100%; }
"""



-- VIEW: baseDataView


{-| base data view
-}
baseDataView : QObjectDetails -> Html Msg
baseDataView obj =
    let
        summary =
            obj.summary
    in
        div [ class "base-data-view" ]
            [ Html.h3 [ class "" ] [ text "Obj" ]
            , summaryView obj.summary
            , methodsView obj
            , propertiesView obj
              --         , text <| toString obj
            ]


{-| CSS parts for baseDataView
-}
baseDataViewCss : String
baseDataViewCss =
    """
.base-data-view {  }

""" ++ summaryViewCss ++ tableViewCss



-- VIEW: summaryView


{-| summary view
-}
summaryView : QObjectSummary -> Html Msg
summaryView summary =
    div [ class "summary-view" ]
        [ div [ class "object-name" ] [ text summary.objectName ]
        , div [ class "class-name" ] [ text summary.className ]
        , div [ class "superclass" ] [ text summary.superClass ]
        , div [ class "kind" ] [ text <| toString summary.objectKind ]
        ]


{-| CSS parts for summaryView
-}
summaryViewCss : String
summaryViewCss =
    """
.summary-view {  }

.summary-view .object-name {  }
.summary-view .class-name {  }
.summary-view .superclass {  }

.summary-view .object-name:before {  content: "Object name: ";  }
.summary-view .class-name:before {  content: "Class name: ";  }
.summary-view .superclass:before { content: "Parent class: "; }
.summary-view .kind:before { content: "Kind: "; }

"""



-- VIEW: tableView


{-| table view
-}
tableView : (a -> List ( String, Html msg )) -> String -> List String -> List a -> Html msg
tableView fn className headers rows =
    let
        toTd ( c, el ) =
            Html.td [ class c ] [ el ]

        toTr x =
            Html.tr [] <| List.map toTd x
    in
        if List.isEmpty rows then
            text ""
        else
            div [ class "table-view" ]
                [ Html.table [ class <| "table table-" ++ className ]
                    [ List.map (\l -> Html.th [] [ text l ]) headers
                        |> Html.thead []
                    , List.map (toTr << fn) rows
                        |> Html.tbody []
                    ]
                ]


{-| CSS parts for tableView
-}
tableViewCss : String
tableViewCss =
    """
.table-view {  }
.table-view table { width:100%; }
.table-view th { font-style: italic; }
"""



-- VIEW: methodsView


{-| methods view
-}
methodsView : QObjectDetails -> Html Msg
methodsView obj =
    let
        row prop =
            [ ( "access", text <| toString prop.access )
            , ( "signature", text prop.signature )
            ]
    in
        tableView row "methods" [ "access", "signature"] obj.methods



-- VIEW: propertiesView


{-| properties view
-}
propertiesView : QObjectDetails -> Html Msg
propertiesView obj =
    let
        row prop =
            [ ( "name", text prop.name )
            , ( "kind", text prop.kind )
            , ( "value", Html.input [ type_ "text", value prop.value ] [] )
            ]
    in
        tableView row "properties" [ "name", "kind", "value" ] obj.properties



--    div [ class "properties-view" ]
--        [
--        ]


{-| CSS parts for propertiesView
-}
propertiesViewCss : String
propertiesViewCss =
    """
.properties-view {  }
"""
