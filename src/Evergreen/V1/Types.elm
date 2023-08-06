module Evergreen.V1.Types exposing (..)

import Dict
import Lamdera


type alias Room =
    { key : String
    , points : Dict.Dict Lamdera.ClientId (Maybe Int)
    }


type alias FrontendModel =
    { room : Maybe Room
    , scoreSelection : Maybe Int
    , enteredRoomCode : String
    , hideStats : Bool
    }


type alias BackendModel =
    Dict.Dict String Room


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
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | KeyCreated Lamdera.ClientId String


type ToFrontend
    = NoOpToFrontend
    | PlanningRoomReceived (Maybe Room)
