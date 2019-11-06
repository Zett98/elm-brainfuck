module Main exposing (init, main)

import Brainfuck.Main exposing (initialVal, updateBrainfuckModel)
import Brainfuck.Types exposing (BrainfuckModel, BrainfuckMsg)
import Brainfuck.View exposing (brainfuckView)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, b, div, li, text, ul)
import Html.Attributes exposing (href)
import Router exposing (Route(..), toRoute)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Route
    , brainfuckModel : BrainfuckModel
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        readyUrl =
            toRoute url
    in
    ( Model key readyUrl initialVal, Cmd.none )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BrainfuckMsg BrainfuckMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = toRoute url }
            , Cmd.none
            )

        BrainfuckMsg brainfuckMsg ->
            ( { model | brainfuckModel = updateBrainfuckModel brainfuckMsg model.brainfuckModel }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ viewLink "/"
        , viewLink "/profile"
        , viewLink "/reviews/the-century-of-the-self"
        , viewLink "/reviews/public-opinion"
        , viewLink "/reviews/shah-of-shahs"
        , viewLink "/brainfuck"
        , pageResolver model.url model
        ]
    }


pageResolver : Route -> Model -> Html Msg
pageResolver url model =
    case url of
        Home ->
            text "home Page"

        BrainfuckPage ->
            viewBrainfuckPage model.brainfuckModel brainfuckView

        _ ->
            text "notFound"


viewBrainfuckPage : BrainfuckModel -> (BrainfuckModel -> Html BrainfuckMsg) -> Html Msg
viewBrainfuckPage model brainfuckView =
    Html.map BrainfuckMsg (brainfuckView model)


viewLink : String -> Html Msg
viewLink path =
    div [] [ a [ href path ] [ text path ] ]
