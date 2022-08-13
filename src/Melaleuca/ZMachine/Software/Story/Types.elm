module Melaleuca.ZMachine.Software.Story.Types exposing (Story, storyDecoder, storyToString)

import Bitwise as Bitwise
import Bytes.Decode as BytesDecode
import Melaleuca.ZMachine.Software.Story.Types.Flags1 as Flags1
import Melaleuca.ZMachine.Software.Story.Types.Flags2 as Flags2


type alias Story =
    { version : Int
    , flags1 : Flags1.Flags1
    , release : Int
    , baseHighMemoryAddress : Int
    , pcStart : Int
    , dictionaryAddress : Int
    , objectTableAddress : Int
    , globalVariablesTableAddress : Int
    , staticBaseMemoryAddress : Int
    , flags2 : Flags2.Flags2
    , serialCodeOrNumber : String
    , abbreviationsTableAddress : Int
    , fileLength : Int
    , checkSum : Int
    , interpreterNumber : Int
    , interpreterVersion : Int
    }


initStory : Story
initStory =
    { version = -1
    , flags1 = Flags1.NoFlags1
    , release = -1
    , baseHighMemoryAddress = -1
    , pcStart = -1
    , dictionaryAddress = -1
    , objectTableAddress = -1
    , globalVariablesTableAddress = -1
    , staticBaseMemoryAddress = -1
    , flags2 = Flags2.NoFlags2
    , serialCodeOrNumber = ""
    , abbreviationsTableAddress = -1
    , fileLength = -1
    , checkSum = -1
    , interpreterNumber = -1
    , interpreterVersion = -1
    }


storyToString : Story -> String
storyToString m =
    "Version: "
        ++ String.fromInt m.version
        ++ ", Flags: "
        ++ Flags1.flags1ToString m.flags1
        ++ ", Release: "
        ++ String.fromInt m.release
        ++ ", High Memory Address: "
        ++ String.fromInt m.baseHighMemoryAddress
        ++ ", PC Start: "
        ++ String.fromInt m.pcStart
        ++ ", Dictionary: "
        ++ String.fromInt m.dictionaryAddress
        ++ ", Serial Code: "
        ++ m.serialCodeOrNumber
        ++ ", Abbreviations: "
        ++ String.fromInt m.abbreviationsTableAddress
        ++ ", File Length: "
        ++ String.fromInt m.fileLength
        ++ ", Checksum: "
        ++ String.fromInt m.checkSum


storyDecoder : BytesDecode.Decoder Story
storyDecoder =
    let
        version =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\v -> { initStory | version = v })

        flags1 story =
            if story.version >= 1 && story.version < 4 then
                Flags1.version1To3FlagDecoder |> BytesDecode.map (\f -> { story | flags1 = Flags1.F1Version1To3 f })

            else
                Flags1.version4To6FlagDecoder |> BytesDecode.map (\f -> { story | flags1 = Flags1.F1Version4To6 f })

        flags2 story =
            Flags2.allVersionsFlags2Decoder |> BytesDecode.map (\f -> { story | flags2 = Flags2.F2AllFlags f })

        release story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | release = r })

        highMemory story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | baseHighMemoryAddress = r })

        pc story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | pcStart = r })

        dictionary story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | dictionaryAddress = r })

        objectTable story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | objectTableAddress = r })

        globalVariables story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | globalVariablesTableAddress = r })

        staticBaseMemory story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | staticBaseMemoryAddress = r })

        serial story =
            let
                readFourChars =
                    BytesDecode.map4 (\a b c d -> [ a, b, c, d ])
                        BytesDecode.unsignedInt8
                        BytesDecode.unsignedInt8
                        BytesDecode.unsignedInt8
                        BytesDecode.unsignedInt8
                        |> BytesDecode.map (List.map Char.fromCode)
                        |> BytesDecode.map String.fromList
            in
            readFourChars
                |> BytesDecode.map (\r -> { story | serialCodeOrNumber = r })

        abbreviations story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | abbreviationsTableAddress = r })

        fileLength story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | fileLength = r })

        checkSum story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | checkSum = r })

        interpreterNumber story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | interpreterNumber = r })

        interpreterVersion story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\r -> { story | interpreterVersion = r })

        readAndSkip f story =
            f story
                |> BytesDecode.andThen (\s -> skip s)

        skip story =
            BytesDecode.unsignedInt8 |> BytesDecode.map (\_ -> story)
    in
    version
        |> BytesDecode.andThen (\s -> flags1 s)
        |> BytesDecode.andThen (readAndSkip release)
        |> BytesDecode.andThen (readAndSkip highMemory)
        |> BytesDecode.andThen (readAndSkip pc)
        |> BytesDecode.andThen (readAndSkip dictionary)
        |> BytesDecode.andThen (readAndSkip objectTable)
        |> BytesDecode.andThen (readAndSkip globalVariables)
        |> BytesDecode.andThen (readAndSkip staticBaseMemory)
        |> BytesDecode.andThen (readAndSkip flags2)
        |> BytesDecode.andThen (\s -> serial s)
        |> BytesDecode.andThen (readAndSkip abbreviations)
        |> BytesDecode.andThen (readAndSkip fileLength)
        |> BytesDecode.andThen (readAndSkip checkSum)
        |> BytesDecode.andThen (readAndSkip interpreterNumber)
        |> BytesDecode.andThen (readAndSkip interpreterVersion)
