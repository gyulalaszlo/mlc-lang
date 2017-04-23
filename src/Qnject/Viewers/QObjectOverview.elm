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
import Qnject.Connection as Connection
import Qnject.Qobject exposing (Address)


-- MODEL


type alias Model =
    { address: Address
    }


initialModel : Address -> Model
initialModel address =
    { address = address
    }


type alias Context =
    { model: Model
    , connection: Connection.Model
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


view : Context -> Html Msg
view ctx =
    div [ class "qobject-view" ]
        [ screenShotView ctx
        ]
        
        
-- CSS

css : String
css = """
.qobject-view {}
""" ++ screenshotViewCss


-- VIEW: screenshotView



{-| screenshot view
-}
screenShotView : Context -> Html Msg
screenShotView {connection, model} =
    let address = model.address
        srcUrl = Connection.url ("/qwidgets/by-address/grab/" ++ address) connection
    in div [ class "screenshot-view" ]
        [ Html.img [ class "screenshot" , src srcUrl ] []
        ]

{-| CSS parts for screenshotView
-}
screenshotViewCss : String
screenshotViewCss = """
.screenshot-view {  }
.screenshot-view img.screenshot { max-width:100%; }
"""


