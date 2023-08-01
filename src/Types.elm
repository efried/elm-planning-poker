module Types exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)


type alias Room =
    { key : String
    , points : List { clientId : ClientId, value : Maybe Int }
    }


type alias FrontendModel =
    { room : Maybe Room
    , scoreSelection : Maybe Int
    , roomCode : String
    }


type alias BackendModel =
    { rooms : Dict String Room
    }


type FrontendMsg
    = NoOpFrontendMsg
    | PlanningRoomCreated
    | ScoreSelected Int
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
