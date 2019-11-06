module Router exposing (Route(..), toRoute)

import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | TodoPage
    | BrainfuckPage
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map TodoPage (s "todos")
        , map BrainfuckPage (s "brainfuck")
        ]


toRoute : Url.Url -> Route
toRoute string =
    Maybe.withDefault NotFound (parse routeParser string)
