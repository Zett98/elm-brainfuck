module Main exposing (init, main)

import Brainfuck.Main exposing (initialVal, updateBrainfuckModel)
import Brainfuck.Types exposing (BrainfuckModel, BrainfuckMsg)
import Brainfuck.View exposing (brainfuckView)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, header, input, label, li, main_, nav, text, ul)
import Html.Attributes exposing (checked, class, for, href, id, type_)
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
            let
                updatedModel =
                    updateBrainfuckModel brainfuckMsg model.brainfuckModel
            in
            ( { model | brainfuckModel = updatedModel }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ main_
            []
            [ viewMenu
            , pageResolver model.url model
            ]
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
    Html.map BrainfuckMsg <| brainfuckView model


viewLink : String -> String -> Html Msg
viewLink path text_ =
    li [ class "menu-item" ] [ a [ href path ] [ text text_ ] ]


viewMenu : Html Msg
viewMenu =
    nav [ class "menu" ]
        [ input [ checked True, class "menu-toggler", id "menu-toggler", type_ "checkbox" ] []
        , label [ for "menu-toggler" ] []
        , ul []
            [ viewLink "/" "Home"
            , viewLink "/brainfuck" "<++>"
            , viewLink "/todos" "Todos"
            , viewLink "/About" "About"
            ]
        ]
