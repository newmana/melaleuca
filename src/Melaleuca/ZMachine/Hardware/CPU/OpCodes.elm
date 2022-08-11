module Melaleuca.ZMachine.Hardware.CPU exposing (Machine, opCodes, opCodesList)


type alias Machine =
    { m : Memory
    }


opCodes : Dict.Dict Int OpCode
opCodes =
    Dict.fromList


opCodesList : List ( Int, OpCode )
opCodesList =
    []
