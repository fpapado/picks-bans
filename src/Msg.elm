module Msg exposing (Msg(..))

import Navigation


type Msg
    = SetMapStatus String
    | AdvancePhase
    | SetRandom
    | NoOp
    | UrlChange Navigation.Location
    | SetMode Int
    | ToggleMapInPlay String
    | SyncPlayMaps
    | SetTeamName Int String
    | SetEventName String
    | RestartSelection
