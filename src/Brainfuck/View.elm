module Brainfuck.View exposing (brainfuckView)

import Brainfuck.Examples exposing (examples)
import Brainfuck.Types exposing (BrainfuckModel, BrainfuckMsg(..), StdOut(..))
import Char
import Html exposing (Html, a, br, button, code, div, h1, p, text, textarea)
import Html.Attributes exposing (class, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import String


brainfuckView : BrainfuckModel -> Html BrainfuckMsg
brainfuckView model =
    div []
        [ h1 [] [ text "Brainfuck interpreter" ]
        , lazy2 viewCode model.generation model.code
        , lazy2 viewStdin model.generation model.stdIn
        , viewControls
        , lazy viewOutput model.stdOut
        , viewExamples
        , p []
            [ text "8-bit cells with overflow. "
            , text "Reading EOF leaves the cell as-is. "
            , text "The interpreter was written in Elm and runs "
            , text "in the browser"
            , text ". Examples adapted from "
            , a
                [ href "https://esolangs.org/wiki/brainfuck" ]
                [ text "esolangs.org" ]
            , text " and "
            , a
                [ href "http://brainfuck.org" ]
                [ text "brainfuck.org" ]
            , text "."
            ]
        ]


viewCode : Int -> String -> Html BrainfuckMsg
viewCode generation code =
    Keyed.node "div"
        [ class "code" ]
        [ ( String.fromInt generation
          , textarea
                [ id "code"
                , onInput Code
                , placeholder "code"
                , value code
                ]
                []
          )
        ]


viewStdin : Int -> String -> Html BrainfuckMsg
viewStdin generation stdin =
    Keyed.node "div"
        [ class "stdin" ]
        [ ( String.fromInt generation
          , Html.textarea
                [ id "stdin"
                , onInput StdIn
                , placeholder "stdin"
                , value stdin
                ]
                []
          )
        ]


viewControls : Html BrainfuckMsg
viewControls =
    div [ class "controls" ]
        [ button [ onClick Run ] [ text "Run" ] ]


output2html : List Int -> List (Html msg)
output2html output =
    let
        newline : Int
        newline =
            Char.toCode '\n'

        trans : Int -> Html msg
        trans n =
            if n == newline then
                br [] []

            else if n < 32 || n > 127 then
                -- non-printable
                code [] [ text <| "<" ++ String.fromInt n ++ ">" ]

            else
                -- printable
                n |> Char.fromCode |> String.fromChar |> Html.text
    in
    List.map trans output


viewOutput : StdOut -> Html BrainfuckMsg
viewOutput stdout =
    let
        output children =
            div [ class "output" ] children
    in
    case stdout of
        Empty ->
            div [] []

        Success stdout_ ->
            output
                [ div [ class "heading" ] [ text "Output" ]
                , div [ class "output" ] (output2html stdout_)
                ]

        Error message ->
            output
                [ div [ class "error" ]
                    [ text <| "Error: " ++ message ]
                ]


viewExamples : Html BrainfuckMsg
viewExamples =
    let
        viewExample example =
            a
                [ href "#", onClick (ShowExample example) ]
                [ text example.name ]

        list =
            List.map viewExample examples
                |> List.intersperse (text ", ")
    in
    div [] (text "Examples: " :: list)
