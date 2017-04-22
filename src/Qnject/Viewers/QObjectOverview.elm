module Qnject.Viewers.QObjectOverview exposing
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

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, src)
import Qnject.Connection exposing (Connection, url)
import Qnject.Qobject exposing (Address)


-- MODEL


type alias Model =
    { connection: Connection
    , address: Address
    }


initialModel : Connection -> Address -> Model
initialModel connection address =
    { connection = connection
    , address = address
    }


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


view : Model -> Html Msg
view model =
    div [ class "qobject-view" ]
        [ text <| toString model
        , screenshotView model
        ]
        
        
-- CSS

css : String
css = """
.qobject-view {}
""" ++ screenshotViewCss


-- VIEW: screenshotView



{-| screenshot view
-}
screenshotView : Model -> Html Msg
screenshotView {connection, address} =
    let srcUrl = url ("/qwidgets/by-address/grab/" ++ address) connection
    in div [ class "screenshot-view" ]
        [ Html.img [ class "screenshot" , src srcUrl ]
            [
            ]
        ]

{-| CSS parts for screenshotView
-}
screenshotViewCss : String
screenshotViewCss = """
.screenshot-view {  }
.screenshot-view img.screenshot { max-width:100%; }
"""


