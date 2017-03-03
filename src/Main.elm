module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, rel, src, style)
import Html.Events exposing (..)
import Task exposing (andThen)


-- Model


type alias Model =
    { maps : List Map
    , currentPhase : Phase
    , currentTeam : Team
    }


type alias Map =
    { id : Int
    , imgurl : String
    , title : String
    , status : Maybe Phase
    }


type Team
    = Team1
    | Team2


type Phase
    = Pick
    | Ban



-- Update


type Msg
    = SetMapStatus Int
    | AdvancePhase


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMapStatus map_id ->
            ( { model
                | maps = List.map (changeMapStatus map_id model.currentPhase) model.maps
              }
            , Cmd.none
            )

        AdvancePhase ->
            let
                ( next_phase, next_team ) =
                    getNextPhase model.currentPhase model.currentTeam
            in
                ( { model
                    | currentPhase = next_phase
                    , currentTeam = next_team
                  }
                , Cmd.none
                )


changeMapStatus : Int -> Phase -> Map -> Map
changeMapStatus map_id phase mp =
    case mp.id == map_id of
        True ->
            { mp | status = Just phase }

        False ->
            mp


getNextPhase : Phase -> Team -> ( Phase, Team )
getNextPhase phase team =
    ( Pick, Team1 )



-- View


view : Model -> Html Msg
view model =
    div [ class "mw7-ns pa3 center" ]
        [ h1 [ class "f2 f1-ns tc sans-serif navy" ] [ text "Picks and Bans" ]
        , div [ class "flex flex-wrap" ]
            (List.map mapView model.maps)
        ]


mapView : Map -> Html Msg
mapView map =
    div [ class "ph2 mb3", style [ ( "flex", "0 0 25%" ) ] ]
        [ img [ src map.imgurl, class "w-100" ] []
        , statusView map.status
        ]


statusView : Maybe Phase -> Html Msg
statusView status =
    let
        mapStateImgUrl =
            case status of
                Just Pick ->
                    "banned.png"

                Just Ban ->
                    "picked.png"

                Nothing ->
                    ""
    in
        img [ src mapStateImgUrl ] []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Init


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { maps = [], currentPhase = Pick, currentTeam = Team1 }


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
