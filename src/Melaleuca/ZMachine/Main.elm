module Melaleuca.ZMachine.Main exposing (Model, Msg(..), init, main, update, view)

import Array as Array
import Browser as Browser
import Bytes as Bytes
import Bytes.Decode as BytesDecode
import Bytes.Encode as BytesEncode
import File as File
import File.Select as FileSelect
import Html as Html
import Html.Attributes as HtmlAttributes
import Html.Events as HtmlEvents
import Melaleuca.ZMachine.Hardware.CPU.OpCodes as OpCodes
import Melaleuca.ZMachine.Hardware.Memory as Memory
import Task


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
    , file : Bytes.Bytes
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0, file = BytesEncode.encode (BytesEncode.unsignedInt8 0) }, Cmd.none )


type Msg
    = Increment
    | Decrement
    | OpenFileClicked
    | FileSelected File.File
    | FileRead Bytes.Bytes


update : Memory.Memory -> Msg -> Model -> ( Model, Cmd Msg )
update m msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        OpenFileClicked ->
            ( model, FileSelect.file [] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileRead (File.toBytes file) )

        FileRead content ->
            ( { model | file = content }, Cmd.none )


view : Memory.Memory -> Model -> Html.Html Msg
view memory model =
    Html.div []
        [ Html.div []
            [ Html.button [ HtmlEvents.onClick Decrement ] [ Html.text "-" ]
            , Html.div [] [ Html.text (String.fromInt model.counter) ]
            , Html.div [] [ Html.text (String.fromInt (Array.get model.counter memory.data |> Maybe.withDefault 42)) ]
            , Html.div [] [ Html.text (String.fromInt (Array.length memory.data)) ]
            , Html.button [ HtmlEvents.onClick Increment ] [ Html.text "+" ]
            ]
        , Html.div
            []
            [ Html.button [ HtmlEvents.onClick OpenFileClicked ] [ Html.text "Open file" ]
            , Html.p [] [ Html.text (BytesDecode.decode OpCodes.machineDecoder model.file |> Maybe.map OpCodes.machineToString |> Maybe.withDefault "Boo") ]
            ]
        ]
