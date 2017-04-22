module Qnject.Main exposing (main)
{-| Describe me please...
-}
import Bsp.DefaultTheme
import Bsp.Msg
import Bsp.Model
import Bsp.Traits exposing (LocalModel)
--import Bsp.Root exposing (subscriptions, update, view)
import Qnject.Client exposing (SharedModel, Msg(..), css, initialModel, initialSharedModel, subscriptions, update, view)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task


init =
    (initialModel, Cmd.none)
--    ( Bsp.Model.modelFrom bspTraits Qnject.Client.initialModel, Cmd.none )
    -- ( initialModel, Task.perform INIT_MSG (Task.succeed MSG_DATA) )


withCss css view model =
    Html.div []
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css"]
            [ Html.text css ]

        , view model
        ]

css = Bsp.DefaultTheme.css ++ Qnject.Client.css


main =
    Html.program
        { init = init
        , view = withCss css view
        , update = update
        , subscriptions = subscriptions
        }


--------------------------

--type alias Msg = Bsp.Msg.Msg ChildMsg ChildModel
--type alias Model = LocalModel ChildMsg ChildModel Qnject.Client.Model
--
--type ChildMsg
--    = OnRefresh
--    | QnjectQAppViewMsg Qnject.QAppView.Msg
--    | QnjectClientMsg Qnject.Client.Msg
--
--
--
--
--type ChildModel
--    = QAppView Qnject.QAppView.Model
--    | B
--
--bspTraits =
--    { subscriptions = always Sub.none
--    , update = childUpdate
--    , view = childView
--
--    , wrapper = Bsp.DefaultTheme.normalTheme toolbar childLabel [QAppView Qnject.QAppView.initialModel,B]
--    }
--
--
--childUpdate msg  model =
--    let {local,shared} = model
--    in case (msg, local) of
--        (QnjectQAppViewMsg msg, QAppView m) ->
--             let (cm, cc) = Qnject.QAppView.update msg m
--             in (QAppView cm, shared, Cmd.map (model.msg << QnjectQAppViewMsg) cc)
--
--
--        _ -> (local, shared, Cmd.none)
--
--
--
--
--{-| child view
---}
--childView : Model -> Html Msg
--childView {local, shared, cursor, msg} =
--
--    Html.div []
--        [ Html.h4 [] [ text <| childLabel local]
--        , case local of
--            QAppView m ->
--                    Maybe.map (\app ->
--                        Html.map (msg << QnjectQAppViewMsg) <|
--                            Qnject.QAppView.view { app = app, model = m}
--
--                        ) shared.app
--                        |> Maybe.withDefault (text "No app")
--
--            B ->
--                Html.div []
--                    [ Html.text  "BBBB"
--                    ]
--
--        ]
--
--
--toolbar : Qnject.Client.Model -> Html ChildMsg
--toolbar model =
--    span
--        [ class "client-toolbar-view" ]
--        [ case model.requestError of
--            Nothing -> text ""
--            Just err ->
--                span [ class "request-errors" ]
--                    [ text <| toString err ]
--
--        , Html.button [onClick OnRefresh] [ text "Refresh" ]
--        ]
--
--
--
--childLabel : ChildModel -> String
--childLabel m =
--    case m of
--        QAppView _ -> "App"
--        B -> "Blank"
