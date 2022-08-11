module Melaleuca.ZMachine.Hardware.Memory exposing (Memory, init)

import Array as Array
import Bytes as Bytes
import Bytes.Encode as BytesEncode


bytesToKiloBytes : Int
bytesToKiloBytes =
    1024


type alias Memory =
    { dataLength : Int
    , data : Array.Array Int
    }


init : Int -> Memory
init dataLength =
    { dataLength = dataLength * bytesToKiloBytes
    , data = Array.initialize (dataLength * bytesToKiloBytes) (\i -> i)
    }
