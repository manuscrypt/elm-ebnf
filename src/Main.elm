module Main exposing (..)

import Html exposing (Html, div, text, input)
import Html.Attributes as HA exposing (type_, value)
import Html.Events as HE
import Parser exposing (..)
import Parsers exposing (grammarParser)
import Types exposing (..)


type alias Model =
    { input : String
    , err : Maybe Parser.Error
    , grammar : Maybe Grammar
    }


type Msg
    = OnInput String


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subs }


init : ( Model, Cmd Msg )
init =
    Model "" Nothing Nothing ! []


parse : Model -> Model
parse model =
    case run Parsers.grammarParser model.input of
        Ok grammer ->
            { model
                | grammar = Just grammer
                , err = Nothing
            }

        Err err ->
            { model
                | grammar = Nothing
                , err = Just err
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        OnInput str ->
            parse { model | input = str } ! []


view : Model -> Html Msg
view model =
    div [ HA.style [ (,) "width" "920px", (,) "margin" "auto" ] ]
        [ Html.textarea
            [ value model.input
            , HE.onInput OnInput
            , HA.rows 6
            ]
            []
        , div [] [ text model.input ]
        , case model.err of
            Nothing ->
                text "no error"

            Just err ->
                viewError err
        , div [] [ Maybe.map viewGrammar model.grammar |> Maybe.withDefault (text "no output") ]
        ]


viewGrammar : Grammar -> Html Msg
viewGrammar g =
    div [] (text "Rules" :: List.map viewRule g)


viewRule : Rule -> Html Msg
viewRule r =
    div [] [ text <| r.lhs ++ " = " ++ toString r.rhs ++ ";" ]


viewRhs : Rhs -> Html msg
viewRhs rhs =
    case rhs of
        Identifier name ->
            div [] [ text <| "Identifier: " ++ name ]

        Terminal str ->
            div [] [ text <| "terminal: " ++ str ]

        _ ->
            div [] [ text <| "Not implemented: " ++ toString rhs ]


viewError : Parser.Error -> Html msg
viewError err =
    div [] [ text <| (toString err.row ++ "/" ++ toString err.col) ++ ":" ++ viewProblem err.problem ]


viewProblem : Problem -> String
viewProblem p =
    case p of
        BadOneOf _ ->
            "bad one of"

        BadIgnore ->
            "bad ignore"

        BadInt ->
            "bad int"

        BadFloat ->
            "bad float"

        ExpectingEnd ->
            "expecting end"

        ExpectingSymbol s ->
            "expected symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expected keyword '" ++ s ++ "'"

        ExpectingVariable ->
            "expected variable"

        Fail s ->
            "failed with: " ++ s


subs : Model -> Sub Msg
subs model =
    Sub.none
