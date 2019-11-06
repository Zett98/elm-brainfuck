module Brainfuck.Eval exposing (Error(..), eval)

import Brainfuck.Parser exposing (Statement(..), parseInput)
import Brainfuck.Tape as Tape exposing (Tape)


type alias EvalState =
    { tape : Tape
    , stdIn : List Int
    , stdOut : List Int
    }


type Error
    = InfiniteLoop
    | TapeOverflow


type alias EvalResult =
    Result Error EvalState


initialState : List Int -> EvalState
initialState stdIn =
    { tape = Tape.empty
    , stdIn = stdIn
    , stdOut = []
    }


eval : String -> List Int -> Result Error (List Int)
eval code stdIn =
    let
        stmts =
            parseInput code

        extractStdOut state =
            List.reverse state.stdOut
    in
    program stmts (initialState stdIn)
        |> Result.map extractStdOut


program : List Statement -> EvalState -> EvalResult
program progr state =
    case progr of
        [] ->
            Ok state

        stmt :: tail ->
            case statement stmt state of
                Ok nextState ->
                    program tail nextState

                Err err ->
                    Err err


statement : Statement -> EvalState -> EvalResult
statement stmt =
    case stmt of
        Left ->
            moveTape Tape.left

        Right ->
            moveTape Tape.right

        Incr ->
            tape Tape.incr

        Decr ->
            tape Tape.decr

        Output ->
            output

        Input ->
            input

        Loop currentScope ->
            loop currentScope


tape : (Tape -> Tape) -> EvalState -> EvalResult
tape fn state =
    Ok { state | tape = fn state.tape }


moveTape : (Tape -> Maybe Tape) -> EvalState -> EvalResult
moveTape fn state =
    case fn state.tape of
        Just newTape ->
            Ok { state | tape = newTape }

        Nothing ->
            Err TapeOverflow


output : EvalState -> EvalResult
output state =
    Ok { state | stdOut = Tape.get state.tape :: state.stdOut }


input : EvalState -> EvalResult
input state =
    Ok <|
        case state.stdIn of
            x :: xs ->
                { state
                    | stdIn = xs
                    , tape = Tape.set x state.tape
                }

            [] ->
                state


loop : List Statement -> EvalState -> EvalResult
loop currentScope state =
    if Tape.get state.tape == 0 then
        Ok state

    else
        case program currentScope state of
            Ok nextState ->
                if nextState /= state then
                    loop currentScope nextState

                else
                    Err InfiniteLoop

            Err msg ->
                Err msg
