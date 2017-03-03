module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, rel, src, style)
import Html.Events exposing (..)
import Task


-- Model
-- TODO: event name


type alias Model =
    { maps : List Map
    , currentModeState : Maybe State
    , currentMode : Int
    , modes : List Mode
    , teams : List Team
    }


type alias Mode =
    { id : Int
    , mtype : ModeType
    , title : String
    , states : List State
    , currentState : Int
    }


type alias State =
    { id : Int
    , phase : Phase
    , teamID : Int
    }


type ModeType
    = Bo3
    | Bo1


type alias Map =
    { id : Int
    , imgurl : String
    , title : String
    , status : Maybe Phase
    }


type alias Team =
    { id : Int
    , name : String
    }


type Phase
    = Pick
    | Ban
    | Rand



-- Update


type Msg
    = SetMapStatus Int
    | AdvancePhase


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modeState =
            model.currentModeState
    in
        case msg of
            SetMapStatus map_id ->
                ( { model
                    | maps = List.map (changeMapStatus map_id modeState) model.maps
                  }
                , Task.perform identity (Task.succeed AdvancePhase)
                )

            AdvancePhase ->
                let
                    currentMode =
                        List.filter (\m -> m.id == model.currentMode) model.modes
                            |> List.head
                            |> Maybe.withDefault bo3Mode

                    nextModeState =
                        getNextModeState modeState currentMode
                in
                    ( { model
                        | currentModeState = nextModeState
                      }
                    , Cmd.none
                    )


changeMapStatus : Int -> Maybe State -> Map -> Map
changeMapStatus map_id maybeState mp =
    case maybeState of
        Just state ->
            case mp.id == map_id of
                True ->
                    { mp | status = Just state.phase }

                False ->
                    mp

        Nothing ->
            mp


getNextModeState : Maybe State -> Mode -> Maybe State
getNextModeState maybeState currentMode =
    case maybeState of
        Just state ->
            let
                nextState =
                    state.id + 1

                nextModeState =
                    currentMode.states
                        |> List.filter (\s -> s.id == nextState)
                        |> List.head
            in
                nextModeState

        Nothing ->
            Nothing



-- View


view : Model -> Html Msg
view model =
    let
        modeState =
            model.currentModeState

        currentTeam =
            case modeState of
                Nothing ->
                    { name = "", id = -1 }

                Just modeS ->
                    List.filter (\t -> t.id == modeS.teamID) model.teams
                        |> List.head
                        |> Maybe.withDefault { name = "", id = -1 }
    in
        div [ class "mw8-ns pa3 center" ]
            [ h1 [ class "f2 f1-ns tc sans-serif navy" ] [ text "Picks and Bans" ]
            , stateView currentTeam modeState
            , div [ class "flex flex-wrap" ]
                (List.map mapView model.maps)
            ]


stateView : Team -> Maybe State -> Html Msg
stateView team maybeState =
    case maybeState of
        Just state ->
            let
                phase =
                    state.phase

                teamText =
                    team.name

                phaseText =
                    case phase of
                        Pick ->
                            "is picking"

                        Ban ->
                            "is banning"

                        Rand ->
                            "Final map selected randomly"
            in
                h2 [ class "f3 f2-ns tc sans-serif" ] [ text <| teamText ++ " " ++ phaseText ]

        Nothing ->
            h2 [ class "f3 f2-ns tc sans-serif" ] [ text "Selection complete" ]


mapView : Map -> Html Msg
mapView map =
    div [ class "ph2 mb3", style [ ( "flex", "0 0 33%" ) ] ]
        [ statusView map.status
        , img
            [ src map.imgurl
            , class "w-100"
            , onClick <| SetMapStatus map.id
            ]
            []
        ]


statusView : Maybe Phase -> Html Msg
statusView status =
    let
        mapStateMark =
            case status of
                Just Pick ->
                    "✔"

                Just Ban ->
                    "✘"

                Just Rand ->
                    "R"

                Nothing ->
                    ""
    in
        span [ class "absolute f1 red" ]
            [ text mapStateMark
            ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Init


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


bo3Mode : Mode
bo3Mode =
    { id = 0, mtype = Bo3, title = "Best of 3", states = bo3States, currentState = 0 }


bo1Mode : Mode
bo1Mode =
    { id = 0, mtype = Bo1, title = "Best of 5", states = bo1States, currentState = 0 }


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
    { maps = initMaps, teams = initTeams, currentModeState = Just initModeState, currentMode = 0, modes = [ bo3Mode, bo1Mode ] }


initModeState : State
initModeState =
    { id = 0
    , phase = Pick
    , teamID = 0
    }


initTeams : List Team
initTeams =
    [ { id = 0, name = "Team 1" }
    , { id = 1, name = "Team 2" }
    ]


initMaps : List Map
initMaps =
    [ { id = 0, imgurl = "images/Cache.jpg", title = "Cache", status = Nothing }
    , { id = 1, imgurl = "images/Cobblestone.jpg", title = "Cobblestone", status = Nothing }
    , { id = 2, imgurl = "images/Dust2.jpg", title = "Dust 2", status = Nothing }
    , { id = 3, imgurl = "images/Inferno.jpg", title = "Inferno", status = Nothing }
    , { id = 4, imgurl = "images/Mirage.jpg", title = "Mirage", status = Nothing }
    , { id = 5, imgurl = "images/Overpass.jpg", title = "Overpass", status = Nothing }
    , { id = 6, imgurl = "images/Train.jpg", title = "Train", status = Nothing }
    ]


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
