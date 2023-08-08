module Evergreen.V3.Types exposing (..)

import Dict
import Lamdera


type alias PointOptions =
    List Int


type alias Room =
    { key : String
    , pointOptions : PointOptions
    , points : Dict.Dict Lamdera.ClientId (Maybe Int)
    }


type alias FrontendModel =
    { room : Maybe Room
    , scoreSelection : Maybe Int
    , enteredRoomCode : String
    , pointOptions : PointOptions
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
    | ChoosePointOptions PointOptions


type ToBackend
    = NoOpToBackend
    | CreatePlanningRoom PointOptions
    | JoinPlanningRoom String
    | LeavePlanningRoom
    | UpdateClientScore String Int
    | ResetRoomScores String


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | KeyCreated Lamdera.ClientId PointOptions String


type ToFrontend
    = NoOpToFrontend
    | PlanningRoomReceived (Maybe Room)
