module Brainfuck.Main exposing (init, main)

import Brainfuck.Eval as Eval
import Brainfuck.Types exposing (Model, Msg(..), StdOut(..))
import Brainfuck.View exposing (view)
import Browser
import Char


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = ""
      , stdIn = ""
      , stdOut = Empty
      , generation = 0
      }
    , Cmd.none
    )


stdoutFromResult : Result Eval.Error (List Int) -> StdOut
stdoutFromResult result =
    case result of
        Ok output ->
            Success output

        Err Eval.InfiniteLoop ->
            Error "Infinite loop detected"

        Err Eval.TapeOverflow ->
            Error "Memory bounds exceeded"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Code code ->
            ( { model | code = code }, Cmd.none )

        StdIn stdin ->
            ( { model | stdIn = stdin }, Cmd.none )

        Run ->
            let
                stdin =
                    model.stdIn |> String.toList |> List.map Char.toCode

                stdout =
                    stdoutFromResult <| Eval.eval model.code stdin
            in
            ( { model | stdOut = stdout }, Cmd.none )

        ShowExample example ->
            ( { model
                | code = example.code
                , stdIn = example.stdIn
                , stdOut = Empty
                , generation = model.generation + 1
              }
            , Cmd.none
            )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
