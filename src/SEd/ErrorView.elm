module SEd.ErrorView exposing
    ( Model
    , initialModel
    , insert
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| An error view for popping up an error message in a timed fashion
-}

import Date
import Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Time exposing (Time)


-- MODEL

{-| A displayed error message
-}
type alias ReportedError error =
    { error: error
    , string: String
    , livesUntil: Time
    }

reportedError : (error -> String) -> Time -> error -> ReportedError error
reportedError toS lifeTime err =
    ReportedError err (toS err) lifeTime


type alias Model =
    { errors: List (ReportedError Error)
    , lastTick: Time
    , lifeTime: Time
    }


initialModel : Model
initialModel =
    { errors = []
    , lastTick = 0
    , lifeTime = 5 * Time.second
    }


insert : Error -> Model -> Model
insert e model =
    let new = reportedError Error.errorToString model.lifeTime e
    in { model | errors = new :: model.errors }


decrementLifetime : Time -> Model -> Model
decrementLifetime t model =
    model.errors
        |> List.map (\err -> { err | livesUntil  = err.livesUntil - t})
        |> List.filter (\err -> err.livesUntil > 0)
        |> (\errs -> { model | errors = errs })

-- MSG


type Msg
    = AddError Error
    | Tick Time



-- SUBSCRIPTIONS

checkInterval : Time
checkInterval = Time.second

subscriptions : Model -> Sub Msg
subscriptions model =
--    Time.every Time.second Tick
    case model.errors of
        [] -> Sub.none
        _ -> Time.every Time.second Tick



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddError err -> insert err model ! []
        Tick t ->
            let deltaT = min checkInterval (abs (t - model.lastTick))
            in decrementLifetime deltaT { model | lastTick = t } ! []


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "error-view" ]
        [ Html.ul [] <| List.map errorRow model.errors
        ]


errorRow : ReportedError Error -> Html Msg
errorRow r =
    Html.li
        [ class "error-row" ]
        [ span [ class "error-msg" ] [ text <| r.string ]
        ]

-- CSS

css : String
css = """
.error-view {}
.error-view ul { list-style: none; padding:0; margin:0;  }

.error-row { background-color: red; color: white; border:3px solid; padding: 0.3em 1em; border-radius: 1em; }
.error-row .error-time {  }

.error-row .error-msg { font-weight: bold; }
.error-row .error-msg:before { content: "ERROR : "; }
"""