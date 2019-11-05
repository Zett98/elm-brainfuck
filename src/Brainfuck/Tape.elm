module Brainfuck.Tape exposing (Tape, decr, defaultOptions, empty, get, incr, left, right, set, withOptions)


type alias Options =
    { maxSize : Int
    , minVal : Int
    , maxVal : Int
    }


type Tape
    = Tape
        { prevCells : List Int
        , currentCell : Int
        , nextCells : List Int
        , size : Int
        , options : Options
        }


defaultOptions : Options
defaultOptions =
    { maxSize = 30000
    , minVal = 0
    , maxVal = 255
    }


withOptions : Options -> Tape
withOptions options =
    Tape
        { prevCells = []
        , currentCell = 0
        , nextCells = []
        , size = 1
        , options = options
        }


get : Tape -> Int
get (Tape tape) =
    tape.currentCell


set : Int -> Tape -> Tape
set value (Tape tape) =
    Tape { tape | currentCell = value }


incr : Tape -> Tape
incr (Tape tape) =
    let
        { currentCell, options } =
            tape

        new =
            if currentCell == options.maxVal then
                options.minVal

            else
                currentCell + 1
    in
    Tape { tape | currentCell = new }


decr : Tape -> Tape
decr (Tape tape) =
    let
        { currentCell, options } =
            tape

        new =
            if currentCell == options.minVal then
                options.maxVal

            else
                currentCell - 1
    in
    Tape { tape | currentCell = new }


left : Tape -> Maybe Tape
left (Tape tape) =
    let
        { prevCells, currentCell, nextCells } =
            tape
    in
    case prevCells of
        value :: rest ->
            Just <|
                Tape
                    { tape
                        | prevCells = rest
                        , currentCell = value
                        , nextCells = currentCell :: nextCells
                    }

        [] ->
            Nothing


right : Tape -> Maybe Tape
right (Tape tape) =
    let
        { prevCells, currentCell, nextCells, size, options } =
            tape
    in
    case nextCells of
        value :: rest ->
            Just <|
                Tape
                    { tape
                        | prevCells = currentCell :: prevCells
                        , currentCell = value
                        , nextCells = rest
                    }

        [] ->
            if size >= options.maxSize then
                Nothing

            else
                Just <|
                    Tape
                        { tape
                            | prevCells = currentCell :: prevCells
                            , currentCell = 0
                            , nextCells = []
                            , size = size + 1
                        }


empty : Tape
empty =
    withOptions defaultOptions
