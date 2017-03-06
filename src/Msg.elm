module Msg exposing (Msg(..))

import Navigation


type Msg
    = SetMapStatus Int
    | AdvancePhase
    | SetRandom
    | NoOp
    | UrlChange Navigation.Location
    | SetMode Int
    | ToggleMapInPlay Int
    | SyncPlayMaps
    | SetTeamName Int String
    | SetEventName String
    | RestartSelection
