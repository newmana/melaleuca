module Melaleuca.ZMachine.Hardware.CPU.OpCodes exposing (Machine, machineDecoder, machineToString, opCodes, opCodesList, toStringDecoder)

import Bytes as Bytes
import Bytes.Decode as BytesDecode
import Dict as Dict
import Melaleuca.ZMachine.Hardware.Memory as Memory


type alias Machine =
    { version : Int
    , error : String
    , memory : Memory.Memory
    }


machineToString : Machine -> String
machineToString m =
    "Version: "
        ++ String.fromInt m.version
        ++ "\nError: "
        ++ m.error


type alias OpCode =
    String


opCodes : Dict.Dict Int OpCode
opCodes =
    Dict.fromList []


opCodesList : List ( Int, OpCode )
opCodesList =
    []


machineDecoder : BytesDecode.Decoder Machine
machineDecoder =
    versionDecoder |> BytesDecode.map (\v -> { version = v, error = "", memory = Memory.init 1 })


toStringDecoder : BytesDecode.Decoder String
toStringDecoder =
    BytesDecode.map toStringForm BytesDecode.unsignedInt8


versionDecoder : BytesDecode.Decoder Int
versionDecoder =
    BytesDecode.unsignedInt8 |> BytesDecode.andThen checkVersion


checkVersion : Int -> BytesDecode.Decoder Int
checkVersion version =
    if (version < 1) || (version > 8) || version == 6 then
        BytesDecode.fail

    else
        BytesDecode.succeed version


toStringForm : Int -> String
toStringForm instruction =
    if instruction > 255 then
        "Invalid"

    else
        String.fromInt instruction
