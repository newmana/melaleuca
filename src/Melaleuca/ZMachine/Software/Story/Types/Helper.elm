module Melaleuca.ZMachine.Software.Story.Types.Helper exposing (fromBool)


fromBool : Bool -> String
fromBool b =
    if b then
        "True"

    else
        "False"
