module Melaleuca.ZMachine.Software.Story.Types.Flags2 exposing (Flags2(..), allVersionsFlags2Decoder, flags2ToString)

import Bitwise as Bitwise
import Bytes.Decode as BytesDecode
import Melaleuca.ZMachine.Software.Story.Types.Helper as Helper
import Melaleuca.ZMachine.Software.Story.Types.StatusLine as StatusLine


type Flags2
    = NoFlags2
    | F2AllFlags AllFlags2


type alias AllFlags2 =
    { transcripting : Bool
    , fixedFont : Bool
    , redrawScreen : Bool
    , usePictures : Bool
    , useUndo : Bool
    , useMouse : Bool
    , useColours : Bool
    , useSoundEffects : Bool
    , useMenus : Bool
    }


flags2ToString : Flags2 -> String
flags2ToString f =
    case f of
        NoFlags2 ->
            "No Flags"

        F2AllFlags fl ->
            "All Flags:\n" ++ allVersionsFlags2ToString fl


allVersionsFlags2ToString : AllFlags2 -> String
allVersionsFlags2ToString { transcripting, fixedFont, redrawScreen, usePictures, useUndo, useMouse, useColours, useSoundEffects, useMenus } =
    "Transcripting: "
        ++ Helper.fromBool transcripting
        ++ ", "
        ++ "Fixed Font: "
        ++ Helper.fromBool fixedFont
        ++ ", "
        ++ "Redraw Screen: "
        ++ Helper.fromBool redrawScreen
        ++ ", "
        ++ "Use Pictures: "
        ++ Helper.fromBool usePictures
        ++ ", "
        ++ "Use Undo: "
        ++ Helper.fromBool useUndo
        ++ ", "
        ++ "Use Mouse: "
        ++ Helper.fromBool useMouse
        ++ ", "
        ++ "Use Colours: "
        ++ Helper.fromBool useColours
        ++ ", "
        ++ "Use Sound Effects: "
        ++ Helper.fromBool useSoundEffects
        ++ ", "
        ++ "Use Menus: "
        ++ Helper.fromBool useMenus


allVersionsFlags2Decoder : BytesDecode.Decoder AllFlags2
allVersionsFlags2Decoder =
    let
        isTranscripting i =
            Bitwise.and 0 i == 1

        isFixedFont i =
            Bitwise.and 1 i == 1

        isRedrawScreen i =
            Bitwise.and 2 i == 1

        isUsePictures i =
            Bitwise.and 4 i == 1

        isUseUndo i =
            Bitwise.and 8 i == 1

        isUseMouse i =
            Bitwise.and 16 i == 1

        isUseColours i =
            Bitwise.and 32 i == 1

        isUseSoundEffects i =
            Bitwise.and 64 i == 1

        isUseMenus i =
            Bitwise.and 128 i == 1
    in
    BytesDecode.unsignedInt8
        |> BytesDecode.map
            (\i ->
                { transcripting = isTranscripting i
                , fixedFont = isFixedFont i
                , redrawScreen = isRedrawScreen i
                , usePictures = isUsePictures i
                , useUndo = isUseUndo i
                , useMouse = isUseMouse i
                , useColours = isUseColours i
                , useSoundEffects = isUseSoundEffects i
                , useMenus = isUseMenus i
                }
            )
