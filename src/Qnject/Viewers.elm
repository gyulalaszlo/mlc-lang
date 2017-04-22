module Qnject.Viewers exposing
    ( Model
    , initialModels

    , appView
    , objectView
    , bspTraits
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| Describe me please...
-}

import Bsp.DefaultTheme
import Bsp.Msg
import Bsp.Traits exposing (LocalModel)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Qnject.Qobject exposing (Address)
import Qnject.ViewerEffects exposing (Effects)
import Qnject.Viewers.QObjectOverview as QObjectOverview
import Qnject.Viewers.QAppView as QAppView

-- MODEL


type Model
    = QAppView QAppView.Model
    | QObjectOverview QObjectOverview.Model


appView : Model
appView = QAppView QAppView.initialModel

objectView : Address -> Model
objectView address =
    QObjectOverview <| QObjectOverview.initialModel address



labelFor : Model -> String
labelFor model =
    case model of
        QAppView _ -> "App"
        QObjectOverview m -> "QObject: " ++ m.address

initialModels : List Model
initialModels =
    [ appView
    ]


--type alias BspViewMsg = Bsp.Msg.Msg Msg Model
--type alias BspLocalModel = LocalModel Msg Model SharedModel
--type alias BspModel = Bsp.Model.Model Msg Model SharedModel Effects
bspTraits =
    { subscriptions = subscriptions
    , update = update
    , view = view

    , wrapper =
        Bsp.DefaultTheme.normalTheme
            labelFor
            (\model -> initialModels)
    }


-- MSG


type Msg
     = QnjectQAppViewMsg QAppView.Msg
     | QnjectQObjectOverviewMsg QObjectOverview.Msg



-- SUBSCRIPTIONS


--subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


--update : Msg -> Model -> (Model, Cmd Msg)
update : Msg -> LocalModel Msg Model s -> (Model, Cmd (Bsp.Msg.Msg Msg Model), Maybe Effects)
update msg model =
    let {local,shared} = model
    in case (msg, local) of
        (QnjectQAppViewMsg msg, QAppView m) ->
             let (cm, cc, ce) = QAppView.update msg m
             in (QAppView cm, Cmd.map (model.msg << QnjectQAppViewMsg) cc, ce)

        (QnjectQObjectOverviewMsg msg, QObjectOverview m) ->
             let (cm, cc) = QObjectOverview.update msg m
             in (QObjectOverview cm, Cmd.map (model.msg << QnjectQObjectOverviewMsg) cc, Nothing)

        _ -> (local, Cmd.none, Nothing)


-- VIEW


--view : Model -> Html Msg
view {local, shared, cursor, msg} =
    let orNoApp = Maybe.withDefault (text "No app")
        inner app =
            case local of
                QAppView m ->
                    QAppView.view { app = app, model = m}
                        |> Html.map QnjectQAppViewMsg

                QObjectOverview m ->
                    QObjectOverview.view m
                        |> Html.map QnjectQObjectOverviewMsg
    in

    Html.div []
        [ Html.h4 [] [ text <| labelFor local]
        , shared.app |> Maybe.map inner |> orNoApp |> Html.map msg
        ]



-- CSS

css : String
css = """
.Viewers-view {}
""" ++ QObjectOverview.css
    ++ QAppView.css
