module Update exposing (..)

import Model exposing (Model, bo3Mode)
import Msg exposing (Msg(..))
import Random
import Random.Extra exposing (sample)
import Route
import Task
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- for manual advance, not used atm
        AdvancePhase ->
            let
                newModel =
                    model
                        |> updateNextModeState

                nextCmd =
                    getNextStateCmd newModel.currentModeState
            in
                ( newModel, nextCmd )

        NoOp ->
            ( model, Cmd.none )

        RestartSelection ->
            ( model
                |> syncPlayMapsFromAll
                |> resetModeState
            , Cmd.none
            )

        SetMapStatus map_id ->
            let
                newPlayMaps =
                    List.map (changeMapStatus map_id model.currentModeState) model.playMaps

                newModel =
                    { model | playMaps = newPlayMaps }
                        |> updateNextModeState

                nextCmd =
                    getNextStateCmd newModel.currentModeState
            in
                ( newModel, nextCmd )

        SetMode newModeID ->
            ( { model | currentMode = newModeID }
                |> resetModeState
                |> syncPlayMapsFromAll
            , Cmd.none
            )

        -- called in nextCmd or manually
        SetRandom ->
            let
                unpickedIDs =
                    List.filter (\m -> m.status == Nothing) model.playMaps
                        |> List.map (\m -> m.id)

                unpickedIDGen =
                    sample unpickedIDs
                        |> Random.map (Maybe.withDefault 1)
            in
                ( model, Random.generate SetMapStatus unpickedIDGen )

        SetEventName newEventName ->
            { model | eventName = newEventName } ! []

        SetTeamName teamID name ->
            let
                newTeams =
                    model.teams
                        |> List.map (changeTeamName teamID name)
            in
                { model | teams = newTeams } ! []

        -- for manual sync, not used atm
        SyncPlayMaps ->
            -- There is some duplication, but since sync is one-way,
            -- it should be fine. This also has the advantage of allowing
            -- selection-specific changes to playMaps, and resets based on
            -- allMaps
            ( model
                |> syncPlayMapsFromAll
                |> resetModeState
            , Cmd.none
            )

        ToggleMapInPlay mapID ->
            let
                newAllMaps =
                    model.allMaps
                        |> List.map (toggleMapInPlay mapID)
            in
                ( { model | allMaps = newAllMaps }
                    |> syncPlayMapsFromAll
                    |> resetModeState
                , Cmd.none
                )

        UrlChange location ->
            let
                newRoute =
                    Route.locFor location
            in
                ( { model | route = newRoute }
                    |> resetModeState
                    |> syncPlayMapsFromAll
                , Cmd.none
                )


toggleMapInPlay : Int -> Map -> Map
toggleMapInPlay id mp =
    case (mp.id == id) of
        True ->
            { mp | inPlay = not mp.inPlay }

        False ->
            mp


changeTeamName : Int -> String -> Team -> Team
changeTeamName id name team =
    case (team.id == id) of
        True ->
            { team | name = name }

        False ->
            team


getNextStateCmd : Maybe State -> Cmd Msg
getNextStateCmd nextModeState =
    case nextModeState of
        Just modeState ->
            case modeState.phase of
                Rand ->
                    Task.perform identity (Task.succeed SetRandom)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


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


updateNextModeState : Model -> Model
updateNextModeState model =
    let
        modeState =
            model.currentModeState

        currentMode =
            List.filter (\m -> m.id == model.currentMode) model.modes
                |> List.head
                |> Maybe.withDefault bo3Mode

        nextModeState =
            getNextModeState modeState currentMode
    in
        { model | currentModeState = nextModeState }


resetModeState : Model -> Model
resetModeState model =
    let
        currentMode =
            model.modes
                |> List.filter (\m -> m.id == model.currentMode)
                |> List.head
    in
        case currentMode of
            Just currentMode ->
                let
                    initModeState =
                        List.head currentMode.states
                in
                    { model | currentModeState = initModeState }

            Nothing ->
                model


syncPlayMapsFromAll : Model -> Model
syncPlayMapsFromAll model =
    let
        newPlayMaps =
            model.allMaps
                |> List.filter (\mp -> mp.inPlay)
    in
        { model | playMaps = newPlayMaps }
