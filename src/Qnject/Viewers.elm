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
import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Qnject.Connection as Connection
import Qnject.Qobject exposing (Address)
import Qnject.ViewerEffects exposing (ViewerEffect)
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
        QAppView _ -> "QApplication"
        QObjectOverview m -> "QObject: " ++ m.address

initialModels : List Model
initialModels =
    [ appView
    ]


bspTraits =
    { subscriptions = subscriptions
    , update = update
    , view = view

    , wrapper =
        Bsp.DefaultTheme.normalTheme
            labelFor
            (\model -> initialModels)
    }

type alias Local s = LocalModel Msg Model s -> Html Msg
type alias BspViewMsg = Bsp.Msg.Msg Msg Model

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
update : Msg -> LocalModel Msg Model s -> (Model, Cmd (Bsp.Msg.Msg Msg Model), Effects ViewerEffect)
update msg model =
    let {local,shared} = model
    in case (msg, local) of
        (QnjectQAppViewMsg msg, QAppView m) ->
             let (cm, cc, ce) = QAppView.update msg m
             in (QAppView cm, Cmd.map (model.msg << QnjectQAppViewMsg) cc, ce)

        (QnjectQObjectOverviewMsg msg, QObjectOverview m) ->
             let (cm, cc) = QObjectOverview.update msg m
             in (QObjectOverview cm, Cmd.map (model.msg << QnjectQObjectOverviewMsg) cc, Effects.none)

        _ -> (local, Cmd.none, Effects.none)


-- VIEW


view : LocalModel Msg Model { a | connection: Connection.Model } -> Html BspViewMsg
view model =
    Html.div []
        [ Html.map model.msg <| localView model
        ]



-- CSS

css : String
css = """
.Viewers-view {}
""" ++ QObjectOverview.css
    ++ QAppView.css



-- VIEW: localView



{-| local view
-}
localView : LocalModel Msg Model {a | connection: Connection.Model } -> Html Msg
localView {local, shared, cursor, msg} =
        case local of
            QAppView m ->
                QAppView.view { connection = shared.connection, model = m}
                    |> Html.map QnjectQAppViewMsg

            QObjectOverview m ->
                QObjectOverview.view { model = m, connection = shared.connection }
                    |> Html.map QnjectQObjectOverviewMsg

{-| CSS parts for localView
-}
localViewCss : String
localViewCss = """
.local-view {  }
"""


