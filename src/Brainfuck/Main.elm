module Brainfuck.Main exposing (initialVal, updateBrainfuckModel)

import Brainfuck.Eval as Eval
import Brainfuck.Types exposing (BrainfuckModel, BrainfuckMsg(..), StdOut(..))
import Char


initialVal : BrainfuckModel
initialVal =
    { code = ""
    , stdIn = ""
    , stdOut = Empty
    , generation = 0
    }


stdoutFromResult : Result Eval.Error (List Int) -> StdOut
stdoutFromResult result =
    case result of
        Ok output ->
            Success output

        Err Eval.InfiniteLoop ->
            Error "Infinite loop detected"

        Err Eval.TapeOverflow ->
            Error "Memory bounds exceeded"


updateBrainfuckModel : BrainfuckMsg -> (BrainfuckModel -> BrainfuckModel)
updateBrainfuckModel msg model =
    case msg of
        Code code ->
            { model | code = code }

        StdIn stdin ->
            { model | stdIn = stdin }

        Run ->
            let
                stdin =
                    model.stdIn |> String.toList |> List.map Char.toCode

                stdout =
                    stdoutFromResult <| Eval.eval model.code stdin
            in
            { model | stdOut = stdout }

        ShowExample example ->
            { model
                | code = example.code
                , stdIn = example.stdIn
                , stdOut = Empty
                , generation = model.generation + 1
            }
