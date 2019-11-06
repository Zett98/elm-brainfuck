module Brainfuck.Types exposing (BrainfuckModel, BrainfuckMsg(..), Example, StdOut(..))


type StdOut
    = Empty
    | Success (List Int)
    | Error String


type alias Example =
    { name : String
    , code : String
    , stdIn : String
    }


type alias BrainfuckModel =
    { code : String
    , stdIn : String
    , stdOut : StdOut
    , generation : Int
    }


type BrainfuckMsg
    = Code String
    | StdIn String
    | ShowExample Example
    | Run
