module AstView.CodeStyleEditor exposing (..)
{-| Describe me please...
|-}


import CAst.CodeStyle exposing (CodeStyle, SideWs(..), IndentWs(..), Ws, defaultCodeStyle)
import Dict
import Html exposing (Html, div, small, span, td, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import List.Extra



-- MODEL


type alias Model =
    { codeStyle: CodeStyle
    , selectedKey: Maybe String
    }


initialModel : Model
initialModel =
    { codeStyle = defaultCodeStyle
    , selectedKey = Nothing
    }


-- MSG


type Msg
    = Select String
    | SetSideWs String Side SideWs
    | SetSideIndent String Side IndentWs

type Side
    = Left
    | Right

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Select k -> { model | selectedKey = Just k } ! []
        SetSideIndent k s i ->
            { model | codeStyle = updateWs k (setSideIndent s i) model.codeStyle } ! []
        SetSideWs k s w ->
            { model | codeStyle = updateWs k (setSideWs s w) model.codeStyle } ! []


updateWs : String -> (Ws -> Ws) -> CodeStyle -> CodeStyle
updateWs key f cs =
    Dict.update key (Maybe.map f) cs

setSideWs : Side -> SideWs -> Ws -> Ws
setSideWs side w ws =
    case side of
        Left -> { ws | left = w }
        Right -> { ws | right = w }

setSideIndent : Side -> IndentWs -> Ws -> Ws
setSideIndent side w ws =
    case side of
        Left -> { ws | indentLeft = w }
        Right -> { ws | indentRight = w }
--
--setSide : (Ws -> (a,a)) -> String -> String  -> a -> CodeStyle -> CodeStyle
--setSide getSides key side val cs =
--    let
--        ws = Dict.get key cs
--            |> Maybe.map (\ws -> getSides ws |> sideWs side)
--
--        sideWs side (left,right) =
--            case side of
--                "left" -> left
--                _ -> right
--    in



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ Html.table
            [ class "table table-code-style"]
            [ ruleHead
            , Html.tbody [] <|
                Dict.foldr (\k v m -> ruleView k v :: m ) [] model.codeStyle
            ]
        ]
--        [ text <| toString model ]

ruleHead : Html Msg
ruleHead =
    Html.thead []
        [ tr []
            [ th [] [ text "Rule" ]
            , th [] [ text "Indent Left" ]
            , th [] [ text "Space Left" ]
            , th [] [ text "Space Right" ]
            , th [] [ text "Indent Right" ]
            ]
        ]

ruleView : String -> Ws -> Html Msg
ruleView key ws =
    tr [ class "code-style-rule"]
       [ td [ class "ws ws-key"] [ text key ]
       , td [class "side-ws indent-ws-left"] [ indentView key Left ws.indentLeft ]
       , td [class "side-ws side-ws-left"] [ sideWsView key Left ws.left ]
       , td [class "side-ws side-ws-right"] [ sideWsView key Right ws.right ]
       , td [class "side-ws indent-ws-right"] [ indentView key Right ws.indentRight ]
       ]


select : (a -> String) -> (a -> msg) -> a -> List a -> Html msg
select f msg v vs =
    let
        fromStr vv = List.Extra.find (\e -> (f e) == vv) vs
            |> Maybe.withDefault v
    in
        Html.select [ onInput (msg << fromStr) ] <|
            List.map
                (\o ->
                    Html.option
                        [ Html.Attributes.selected (v == o) ]
                        [ text <| f o ])
                vs

sideWsToString : SideWs -> String
sideWsToString w =
    case w of
        Space -> "_ Space"
        NoSpace -> ""
        LineBreak -> "⏎ Line"

sideWsView : String -> Side -> SideWs -> Html Msg
sideWsView key side w =
    div [class "side-ws-view"]
        [ select sideWsToString (SetSideWs key side) w [Space, NoSpace, LineBreak] ]


indentToString : IndentWs -> String
indentToString w =
    case w of
        Indent -> "In ->"
        Outdent -> "<- Out"
        NoIndent -> ""


indentView : String -> Side -> IndentWs -> Html Msg
indentView key side w =
    div [class "side-ws-view"]
        [ select indentToString (SetSideIndent key side) w [NoIndent, Indent, Outdent] ]
--        case w of
--            Indent -> [ text "Indent"]
--            Outdent -> [ text "Outdent"]
--            NoIndent -> [ text "No indent"]
