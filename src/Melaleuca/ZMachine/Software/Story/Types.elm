module Melaleuca.ZMachine.Software.Story.Types exposing (Story, storyDecoder, storyToString)

import Bitwise as Bitwise
import Bytes.Decode as BytesDecode
import Melaleuca.ZMachine.Software.Story.Types.Flags1 as Flags1


type alias Story =
    { version : Int
    , flags1 : Flags1.Flags1
    , release : Int
    , baseMemoryAddress : Int
    , pcStart : Int
    , mainRoutineAddress : Int
    , dictionaryAddress : Int
    , objectTableAddress : Int
    , globalVariablesTableAddress : Int
    , staticBaseMemoryAddress : Int
    }


initStory : Story
initStory =
    { version = -1
    , flags1 = Flags1.NoFlags1
    , release = -1
    , baseMemoryAddress = -1
    , pcStart = -1
    , mainRoutineAddress = -1
    , dictionaryAddress = -1
    , objectTableAddress = -1
    , globalVariablesTableAddress = -1
    , staticBaseMemoryAddress = -1
    }


storyToString : Story -> String
storyToString m =
    "Version: "
        ++ String.fromInt m.version
        ++ ", Flags: "
        ++ Flags1.flags1ToString m.flags1
        ++ ", Release: "
        ++ String.fromInt m.release


storyDecoder : BytesDecode.Decoder Story
storyDecoder =
    let
        version =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\v -> { initStory | version = v })

        flags story =
            if story.version >= 1 && story.version < 4 then
                Flags1.version1To3FlagDecoder |> BytesDecode.map (\f -> { story | flags1 = Flags1.F1Version1To3 f })

            else
                Flags1.version4To6FlagDecoder |> BytesDecode.map (\f -> { story | flags1 = Flags1.F1Version4To6 f })

        release story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | release = r })

        skip story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\_ -> story)
    in
    version
        |> BytesDecode.andThen (\s -> flags s)
        |> BytesDecode.andThen (\s -> release s)
        |> BytesDecode.andThen (\s -> skip s)
