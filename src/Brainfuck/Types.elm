module Brainfuck.Types exposing (Example, Model, Msg(..), StdOut(..))


type StdOut
    = Empty
    | Success (List Int)
    | Error String


type alias Example =
    { name : String
    , code : String
    , stdIn : String
    }


type alias Model =
    { code : String
    , stdIn : String
    , stdOut : StdOut
    , generation : Int
    }


type Msg
    = Code String
    | StdIn String
    | ShowExample Example
    | Run
