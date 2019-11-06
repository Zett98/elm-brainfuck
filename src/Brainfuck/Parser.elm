module Brainfuck.Parser exposing (Statement(..), parseInput)


type Statement
    = Incr
    | Decr
    | Output
    | Input
    | Left
    | Right
    | Loop (List Statement)


{-| record ParserState
where
currentScope = content of the current scope ( top-level | loop)
outerScope = outer loop stack
|
-}
type alias ParserState =
    { currentScope : List Statement
    , outerScope : List (List Statement)
    }



-- HELPERS --


startLoop : ParserState -> ParserState
startLoop state =
    { state
        | outerScope = state.currentScope :: state.outerScope
        , currentScope = []
    }


endLoop : ParserState -> ParserState
endLoop state =
    case state.outerScope of
        x :: xs ->
            { state
                | currentScope = Loop (List.reverse state.currentScope) :: x
                , outerScope = xs
            }

        [] ->
            state



-- update ParserState


sendCommand : Statement -> ParserState -> ParserState
sendCommand stmt state =
    { state | currentScope = stmt :: state.currentScope }


{-|

    close down loops that had no ending

|

-}
closeOuterScope : ParserState -> ParserState
closeOuterScope state =
    case state.outerScope of
        [] ->
            state

        _ ->
            -- close unterminated loop
            closeOuterScope (endLoop state)



-- parse statements by Chars


statement : Char -> ParserState -> ParserState
statement char =
    case char of
        '-' ->
            sendCommand Decr

        '+' ->
            sendCommand Incr

        '>' ->
            sendCommand Right

        '<' ->
            sendCommand Left

        '.' ->
            sendCommand Output

        ',' ->
            sendCommand Input

        '[' ->
            startLoop

        ']' ->
            endLoop

        _ ->
            identity


emptyState : ParserState
emptyState =
    { currentScope = []
    , outerScope = []
    }


parseInput : String -> List Statement
parseInput input =
    let
        state =
            List.foldl statement emptyState (String.toList input)

        { currentScope } =
            closeOuterScope state
    in
    List.reverse currentScope
