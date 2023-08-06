module Types exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)


type alias Room =
    { key : String
    , points : Dict ClientId (Maybe Int)
    }


type alias FrontendModel =
    { room : Maybe Room
    , scoreSelection : Maybe Int
    , enteredRoomCode : String
    , hideStats : Bool
    }


type alias BackendModel =
    Dict String Room


type FrontendMsg
    = NoOpFrontendMsg
    | PlanningRoomCreated
    | ScoreSelected Room Int
    | RoomCodeEntered String
    | RequestPlanningRoom
    | LeftPlanningRoom
    | ToggleStats
    | ResetRoom Room


type ToBackend
    = NoOpToBackend
    | CreatePlanningRoom
    | JoinPlanningRoom String
    | LeavePlanningRoom
    | UpdateClientScore String Int
    | ResetRoomScores String


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | KeyCreated ClientId String


type ToFrontend
    = NoOpToFrontend
    | PlanningRoomReceived (Maybe Room)
