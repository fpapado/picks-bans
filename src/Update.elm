module Update exposing (..)

import Model exposing (Model, bo3Mode)
import Msg exposing (Msg(..))
import Random
import Random.Extra exposing (sample)
import Route
import Task
import Types exposing (..)
import Dict exposing (Dict)


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
                changeStatus =
                    Maybe.map (\mp -> changeMapStatus model.currentModeState mp)

                newPlayMaps =
                    Dict.update map_id changeStatus model.playMaps

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
                    Dict.filter (\id m -> m.status == Nothing) model.playMaps
                        |> Dict.keys

                unpickedIDGen =
                    sample unpickedIDs
                        |> Random.map (Maybe.withDefault "")
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
                    model.allMaps |> toggleMapInPlay mapID
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
                    |> syncPlayMapsFromAll
                    |> resetModeState
                , Cmd.none
                )


toggleMapInPlay : String -> Dict String Map -> Dict String Map
toggleMapInPlay id maps =
    let
        toggle =
            Maybe.map (\mp -> { mp | inPlay = not mp.inPlay })
    in
        Dict.update id toggle maps


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


changeMapStatus : Maybe State -> Map -> Map
changeMapStatus maybeState mp =
    case maybeState of
        Just state ->
            { mp | status = Just state.phase }

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
                |> Dict.filter (\id mp -> mp.inPlay)
    in
        { model | playMaps = newPlayMaps }
