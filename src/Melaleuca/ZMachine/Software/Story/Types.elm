module Melaleuca.ZMachine.Software.Story.Types exposing (Story, storyDecoder, storyToString)

import Bitwise as Bitwise
import Bytes.Decode as BytesDecode


type alias Story =
    { version : Int
    , flags : Flags
    }


type StatusLine
    = Scored
    | Timed


statusLineToString : StatusLine -> String
statusLineToString sl =
    case sl of
        Scored ->
            "Scored"

        Timed ->
            "Timed"


type alias Version1To3Flags =
    { statusLineType : StatusLine
    , twoDiscs : Bool
    , isTandy : Bool
    , statusLine : Bool
    , screenSplit : Bool
    , proportionalFont : Bool
    }


fromBool : Bool -> String
fromBool b =
    if b then
        "True"

    else
        "False"


version1To3FlagsToString : Version1To3Flags -> String
version1To3FlagsToString { statusLineType, twoDiscs, isTandy, statusLine, screenSplit, proportionalFont } =
    statusLineToString statusLineType
        ++ ", "
        ++ "Two Discs: "
        ++ fromBool twoDiscs
        ++ ", "
        ++ "Tandy: "
        ++ fromBool isTandy
        ++ ", "
        ++ "Status Line: "
        ++ fromBool statusLine
        ++ ", "
        ++ "Screen Split: "
        ++ fromBool screenSplit
        ++ ", "
        ++ "Proportional Font: "
        ++ fromBool proportionalFont


type alias Verison4To6Flags =
    { colours : Bool
    , displayPicture : Bool
    , boldFaceAvailable : Bool
    , italicFaceAvailable : Bool
    , fixedFont : Bool
    , timedKeyboard : Bool
    }


verison4To6FlagsToString : Verison4To6Flags -> String
verison4To6FlagsToString { colours, displayPicture, boldFaceAvailable, italicFaceAvailable, fixedFont, timedKeyboard } =
    "Colours: "
        ++ fromBool colours
        ++ ", "
        ++ "Display Picture: "
        ++ fromBool displayPicture
        ++ ", "
        ++ "Bold Face: "
        ++ fromBool boldFaceAvailable
        ++ ", "
        ++ "Italic Face: "
        ++ fromBool italicFaceAvailable
        ++ ", "
        ++ "Fixed Font: "
        ++ fromBool fixedFont
        ++ ", "
        ++ "Timed Keyboard: "
        ++ fromBool timedKeyboard


type Flags
    = NoFlags
    | Version1To3 Version1To3Flags
    | Version4To6 Verison4To6Flags


flagsToString : Flags -> String
flagsToString f =
    case f of
        NoFlags ->
            "No Flags"

        Version1To3 fl ->
            "Version 1-3 Flags:\n" ++ version1To3FlagsToString fl

        Version4To6 fl ->
            "Version 4-6 Flags:\n" ++ verison4To6FlagsToString fl


storyToString : Story -> String
storyToString m =
    "Version: "
        ++ String.fromInt m.version
        ++ ", Flags: "
        ++ flagsToString m.flags


storyDecoder : BytesDecode.Decoder Story
storyDecoder =
    let
        version =
            versionDecoder |> BytesDecode.map (\v -> { version = v, flags = NoFlags })

        flags story =
            if story.version >= 1 && story.version < 4 then
                version1To3FlagDecoder |> BytesDecode.map (\f -> { story | flags = Version1To3 f })

            else
                version4To6FlagDecoder |> BytesDecode.map (\f -> { story | flags = Version4To6 f })
    in
    version |> BytesDecode.andThen (\s -> flags s)


versionDecoder : BytesDecode.Decoder Int
versionDecoder =
    BytesDecode.unsignedInt8


version1To3FlagDecoder : BytesDecode.Decoder Version1To3Flags
version1To3FlagDecoder =
    let
        getLineType i =
            if Bitwise.and i 2 == 1 then
                Timed

            else
                Scored

        hasTwoDiscs i =
            Bitwise.and 4 i == 1

        isTandy i =
            Bitwise.and 8 i == 1

        hasStatusLine i =
            Bitwise.and 16 i == 1

        hasScreenSplit i =
            Bitwise.and 32 i == 1

        hasProportionalFont i =
            Bitwise.and 64 i == 1
    in
    BytesDecode.unsignedInt8
        |> BytesDecode.map
            (\i ->
                { statusLineType = getLineType i
                , twoDiscs = hasTwoDiscs i
                , isTandy = isTandy i
                , statusLine = hasStatusLine i
                , screenSplit = hasScreenSplit i
                , proportionalFont = hasProportionalFont i
                }
            )


version4To6FlagDecoder : BytesDecode.Decoder Verison4To6Flags
version4To6FlagDecoder =
    let
        hasColours i =
            Bitwise.and 4 i == 1

        displayPicture i =
            Bitwise.and 16 i == 1

        boldFaceAvailable i =
            Bitwise.and 32 i == 1

        italicFaceAvailable i =
            Bitwise.and 64 i == 1

        fixedFont i =
            Bitwise.and 128 i == 1

        timedKeyboard i =
            Bitwise.and 255 i == 1
    in
    BytesDecode.unsignedInt8
        |> BytesDecode.map
            (\i ->
                { colours = hasColours i
                , displayPicture = displayPicture i
                , boldFaceAvailable = boldFaceAvailable i
                , italicFaceAvailable = italicFaceAvailable i
                , fixedFont = fixedFont i
                , timedKeyboard = timedKeyboard i
                }
            )
