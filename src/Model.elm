module Model exposing (Model, init, links, bo3Mode, bo1Mode)

import Msg exposing (Msg)
import Route
import Navigation
import Types exposing (..)


type alias Model =
    { allMaps : List Map
    , playMaps : List Map
    , currentModeState : Maybe State
    , currentMode : Int
    , modes : List Mode
    , teams : List Team
    , route : Route.Model
    , eventName : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel, Cmd.none )


links : List ( Route.Location, String )
links =
    [ ( Route.Setup, "Setup" )
    , ( Route.Play, "Selection" )
    ]


bo3Mode : Mode
bo3Mode =
    { id = 0, mtype = Bo3, title = "Best of 3", states = bo3States }


bo1Mode : Mode
bo1Mode =
    { id = 1, mtype = Bo1, title = "Best of 1", states = bo1States }


bo3States : List State
bo3States =
    [ { id = 0, teamID = 0, phase = Ban }
    , { id = 1, teamID = 1, phase = Ban }
    , { id = 2, teamID = 1, phase = Pick }
    , { id = 3, teamID = 0, phase = Pick }
    , { id = 4, teamID = -1, phase = Rand }
    ]


bo1States : List State
bo1States =
    [ { id = 0, teamID = 0, phase = Ban }
    , { id = 1, teamID = 1, phase = Ban }
    , { id = 2, teamID = 1, phase = Ban }
    , { id = 3, teamID = 0, phase = Ban }
    , { id = 4, teamID = -1, phase = Rand }
    ]


initModel : Model
initModel =
    { playMaps = []
    , allMaps = initMaps
    , teams = initTeams
    , currentModeState = Just initModeState
    , currentMode = 0
    , modes = [ bo3Mode, bo1Mode ]
    , route = Route.init (Just Route.Setup)
    , eventName = ""
    }


initModeState : State
initModeState =
    List.head bo3States
        |> Maybe.withDefault { id = 0, teamID = 0, phase = Ban }


initTeams : List Team
initTeams =
    [ { id = 0, name = "Team 1" }
    , { id = 1, name = "Team 2" }
    ]


initMaps : List Map
initMaps =
    [ { id = 0, imgurl = "images/Cache.png", title = "Cache", status = Nothing, inPlay = False }
    , { id = 1, imgurl = "images/Cbble.png", title = "Cobblestone", status = Nothing, inPlay = False }
    , { id = 2, imgurl = "images/Dust_2.png", title = "Dust 2", status = Nothing, inPlay = False }
    , { id = 3, imgurl = "images/Inferno.png", title = "Inferno", status = Nothing, inPlay = False }
    , { id = 4, imgurl = "images/Mirage.png", title = "Mirage", status = Nothing, inPlay = False }
    , { id = 5, imgurl = "images/Nuke.png", title = "Nuke", status = Nothing, inPlay = False }
    , { id = 6, imgurl = "images/Overpass.png", title = "Overpass", status = Nothing, inPlay = False }
    , { id = 7, imgurl = "images/Train.png", title = "Train", status = Nothing, inPlay = False }
    ]
