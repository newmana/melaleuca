module Melaleuca.ZMachine.Software.Story.Types.Flags1 exposing (Flags1(..), flags1ToString, version1To3FlagDecoder, version4To6FlagDecoder)

import Bitwise as Bitwise
import Bytes.Decode as BytesDecode
import Melaleuca.ZMachine.Software.Story.Types.Helper as Helper
import Melaleuca.ZMachine.Software.Story.Types.StatusLine as StatusLine


type Flags1
    = NoFlags1
    | F1Version1To3 Version1To3Flags1
    | F1Version4To6 Verison4To6Flags1


type alias Version1To3Flags1 =
    { statusLineType : StatusLine.StatusLine
    , twoDiscs : Bool
    , isTandy : Bool
    , statusLine : Bool
    , screenSplit : Bool
    , proportionalFont : Bool
    }


flags1ToString : Flags1 -> String
flags1ToString f =
    case f of
        NoFlags1 ->
            "No Flags"

        F1Version1To3 fl ->
            "Version 1-3 Flags:\n" ++ version1To3FlagsToString fl

        F1Version4To6 fl ->
            "Version 4-6 Flags:\n" ++ verison4To6FlagsToString fl


version1To3FlagsToString : Version1To3Flags1 -> String
version1To3FlagsToString { statusLineType, twoDiscs, isTandy, statusLine, screenSplit, proportionalFont } =
    StatusLine.statusLineToString statusLineType
        ++ ", "
        ++ "Two Discs: "
        ++ Helper.fromBool twoDiscs
        ++ ", "
        ++ "Tandy: "
        ++ Helper.fromBool isTandy
        ++ ", "
        ++ "Status Line: "
        ++ Helper.fromBool statusLine
        ++ ", "
        ++ "Screen Split: "
        ++ Helper.fromBool screenSplit
        ++ ", "
        ++ "Proportional Font: "
        ++ Helper.fromBool proportionalFont


type alias Verison4To6Flags1 =
    { colours : Bool
    , displayPicture : Bool
    , boldFaceAvailable : Bool
    , italicFaceAvailable : Bool
    , fixedFont : Bool
    , timedKeyboard : Bool
    }


verison4To6FlagsToString : Verison4To6Flags1 -> String
verison4To6FlagsToString { colours, displayPicture, boldFaceAvailable, italicFaceAvailable, fixedFont, timedKeyboard } =
    "Colours: "
        ++ Helper.fromBool colours
        ++ ", "
        ++ "Display Picture: "
        ++ Helper.fromBool displayPicture
        ++ ", "
        ++ "Bold Face: "
        ++ Helper.fromBool boldFaceAvailable
        ++ ", "
        ++ "Italic Face: "
        ++ Helper.fromBool italicFaceAvailable
        ++ ", "
        ++ "Fixed Font: "
        ++ Helper.fromBool fixedFont
        ++ ", "
        ++ "Timed Keyboard: "
        ++ Helper.fromBool timedKeyboard


version1To3FlagDecoder : BytesDecode.Decoder Version1To3Flags1
version1To3FlagDecoder =
    let
        getLineType i =
            if Bitwise.and i 2 == 1 then
                StatusLine.Timed

            else
                StatusLine.Scored

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


version4To6FlagDecoder : BytesDecode.Decoder Verison4To6Flags1
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
