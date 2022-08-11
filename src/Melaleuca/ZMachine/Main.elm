module Melaleuca.ZMachine.Main exposing (Model, Msg(..), init, main, update, view)

import Array as Array
import Browser as Browser
import Html as Html
import Html.Attributes as HtmlAttributes
import Html.Events as HtmlEvents
import Melaleuca.ZMachine.Hardware.Memory as Memory


main : Platform.Program {} Model Msg
main =
    let
        m =
            Memory.init 64
    in
    Browser.element
        { init = always init
        , update = update m
        , view = view m
        , subscriptions = always Sub.none
        }


type alias Model =
    { counter : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0 }, Cmd.none )


type Msg
    = Increment
    | Decrement


update : Memory.Memory -> Msg -> Model -> ( Model, Cmd Msg )
update m msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )


view : Memory.Memory -> Model -> Html.Html Msg
view memory model =
    Html.div []
        [ Html.button [ HtmlEvents.onClick Decrement ] [ Html.text "-" ]
        , Html.div [] [ Html.text (String.fromInt model.counter) ]
        , Html.div [] [ Html.text (String.fromInt (Array.get model.counter memory.data |> Maybe.withDefault 42)) ]
        , Html.div [] [ Html.text (String.fromInt (Array.length memory.data)) ]
        , Html.button [ HtmlEvents.onClick Increment ] [ Html.text "+" ]
        ]
