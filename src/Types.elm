module Types exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)


type Score
    = One
    | Two
    | Four
    | Eight
    | Sixteen


scoreToInt : Score -> Int
scoreToInt score =
    case score of
        One ->
            1

        Two ->
            2

        Four ->
            4

        Eight ->
            8

        Sixteen ->
            16


type alias Room =
    { key : String
    , points : List { clientId : ClientId, value : Maybe Score }
    }


type alias FrontendModel =
    { room : Maybe Room
    , scoreSelection : Score
    , roomCode : String
    }


type alias BackendModel =
    { rooms : Dict String Room
    }


type FrontendMsg
    = NoOpFrontendMsg
    | PlanningRoomCreated
    | ScoreSelected Score
    | RoomCodeEntered String
    | RequestPlanningRoom


type ToBackend
    = NoOpToBackend
    | CreatePlanningRoom
    | JoinPlanningRoom String


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | KeyCreated ClientId String


type ToFrontend
    = NoOpToFrontend
    | PlanningRoomReceived (Maybe Room)
