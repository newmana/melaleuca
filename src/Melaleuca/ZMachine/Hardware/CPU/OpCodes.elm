module Melaleuca.ZMachine.Hardware.CPU.OpCodes exposing (opCodes, opCodesList)

import Bytes as Bytes
import Dict as Dict
import Melaleuca.ZMachine.Hardware.Memory as Memory


type alias OpCode =
    String


opCodes : Dict.Dict Int OpCode
opCodes =
    Dict.fromList []


opCodesList : List ( Int, OpCode )
opCodesList =
    []
