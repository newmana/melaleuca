module Melaleuca.ZMachine.Software.Story.Types.StatusLine exposing (StatusLine(..), statusLineToString)


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
