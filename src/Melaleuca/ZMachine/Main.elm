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
import Melaleuca.ZMachine.Software.Story.Types as StoryTypes
import Task


main : Platform.Program {} Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { counter : Int
    , file : Bytes.Bytes
    , memory : Memory.Memory
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0, file = BytesEncode.encode (BytesEncode.unsignedInt8 0), memory = Memory.init 0 }, Cmd.none )


type Msg
    = Increment
    | Decrement
    | OpenFileClicked
    | FileSelected File.File
    | FileRead Bytes.Bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            let
                listStep : BytesDecode.Decoder a -> ( Int, Array.Array a ) -> BytesDecode.Decoder (BytesDecode.Step ( Int, Array.Array a ) (Array.Array a))
                listStep decoder ( n, arr ) =
                    if n <= 0 then
                        BytesDecode.succeed (BytesDecode.Done arr)

                    else
                        BytesDecode.map (\a -> BytesDecode.Loop ( n - 1, Array.append arr (Array.fromList [ a ]) )) decoder

                x =
                    BytesDecode.loop ( Bytes.width content, Array.empty ) (listStep BytesDecode.unsignedInt8)

                y =
                    BytesDecode.decode x content |> Maybe.withDefault Array.empty

                newMemory =
                    { data = y }
            in
            ( { model | file = content, memory = newMemory }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button [ HtmlEvents.onClick Decrement ] [ Html.text "-" ]
            , Html.div [] [ Html.text (String.fromInt model.counter) ]
            , Html.div [] [ Html.text (String.fromInt (Array.length model.memory.data)) ]
            , Html.button [ HtmlEvents.onClick Increment ] [ Html.text "+" ]
            ]
        , Html.div
            []
            [ Html.button [ HtmlEvents.onClick OpenFileClicked ] [ Html.text "Open file" ]
            , Html.p [] [ Html.text (BytesDecode.decode StoryTypes.storyDecoder model.file |> Maybe.map StoryTypes.storyToString |> Maybe.withDefault "Boo") ]
            ]
        ]
