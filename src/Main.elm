module Main exposing (Model, Msg(..), init, main, parse, update, view, viewError, viewExpression, viewGrammar, viewProblem, viewRule)

-- import BnfParsers as Parsers exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (value)
import Html.Events as HE
import Parser exposing (Problem(..), run)
import WsnParsers as Parsers exposing (Expression, Factor(..), Production, Syntax(..), Term)


type alias Model =
    { input : String
    , err : Maybe (List Parser.DeadEnd)
    , grammar : Maybe Syntax
    }


type Msg
    = OnInput String


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, view = view, subscriptions = always Sub.none }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing Nothing, Cmd.none )


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
            ( parse { model | input = str }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ HA.style "width" "920px", HA.style "margin" "auto" ]
        [ Html.textarea
            [ value model.input
            , HE.onInput OnInput
            , HA.rows 12
            , HA.style "width" "100%"
            ]
            []
        , div [] [ text model.input ]
        , case model.err of
            Nothing ->
                text "no error"

            Just err ->
                div [] <| List.map viewError err
        , div [] [ Maybe.map viewGrammar model.grammar |> Maybe.withDefault (text "no output") ]
        ]


viewGrammar : Syntax -> Html Msg
viewGrammar (Syntax productions) =
    div [] (text "Rules" :: List.map viewRule productions)


viewRule : Production -> Html Msg
viewRule { identifier, expression } =
    div [] [ text <| identifier ++ " = " ++ Debug.toString expression ++ ";" ]


viewExpression : Expression -> Html msg
viewExpression exp =
    div [] <| List.map viewTerm exp


viewTerm : Term -> Html msg
viewTerm term =
    div [] <| List.map viewFactor term


viewFactor : Factor -> Html msg
viewFactor f =
    div [] <|
        [ case f of
            Id id ->
                Html.span [] [ text <| "id(" ++ id ++ ")" ]

            Literal lit ->
                Html.span [] [ text <| "lit(" ++ lit ++ ")" ]

            Repetition _ ->
                Html.span [] [ text <| "repetition" ]

            Option _ ->
                Html.span [] [ text <| "option" ]

            Group _ ->
                Html.span [] [ text <| "grouping" ]
        ]


viewError : Parser.DeadEnd -> Html msg
viewError err =
    div [] [ text <| (Debug.toString err.row ++ "/" ++ Debug.toString err.col) ++ ":" ++ viewProblem err.problem ]


viewProblem : Parser.Problem -> String
viewProblem p =
    case p of
        Expecting what ->
            "expecting string: " ++ what

        ExpectingInt ->
            "bad int"

        ExpectingFloat ->
            "bad float"

        ExpectingEnd ->
            "expecting end"

        ExpectingSymbol s ->
            "expected symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expected keyword '" ++ s ++ "'"

        ExpectingVariable ->
            "expected variable"

        Problem s ->
            "failed with: " ++ s

        BadRepeat ->
            "you made a bad repeat"

        ExpectingHex ->
            "bad hex"

        ExpectingOctal ->
            "bad octal"

        ExpectingBinary ->
            "bad binary"

        ExpectingNumber ->
            "bad number"

        UnexpectedChar ->
            "unexpected char"
