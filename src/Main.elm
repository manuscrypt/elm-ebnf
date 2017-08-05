module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes as HA exposing (type_, value)
import Html.Events as HE
import Parser exposing (..)
import Parsers exposing (..)


type alias Model =
    { input : String
    , err : Maybe Parser.Error
    , grammar : Maybe Syntax
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
    case run Parsers.syntax model.input of
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
    case msg of
        OnInput str ->
            parse { model | input = str } ! []


view : Model -> Html Msg
view model =
    div [ HA.style [ (,) "width" "920px", (,) "margin" "auto" ] ]
        [ Html.textarea
            [ value model.input
            , HE.onInput OnInput
            , HA.rows 12
            , HA.style [ (,) "width" "100%" ]
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


viewGrammar : Syntax -> Html Msg
viewGrammar (Syntax productions) =
    div [] (text "Rules" :: List.map viewRule productions)


viewRule : Production -> Html Msg
viewRule (Production factor expression) =
    div [] [ text <| toString factor ++ " = " ++ toString expression ++ ";" ]


viewRhs : Factor -> Html msg
viewRhs rhs =
    case rhs of
        Identifier name ->
            div [] [ text <| "Identifier: " ++ name ]

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

        ExpectingClosing what ->
            "expecting closing: " ++ what

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

        BadRepeat ->
            "you made a bad repeat"


subs : Model -> Sub Msg
subs model =
    Sub.none
