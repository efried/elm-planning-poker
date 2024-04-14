module Types exposing (BackendModel, BackendMsg(..), CardOptions, FrontendModel, FrontendMsg(..), Game, ToBackend(..), ToFrontend(..))

import Dict exposing (Dict)
import Element exposing (Device)
import Lamdera exposing (ClientId, Key, SessionId)


type alias CardOptions =
    List Int


type alias Game =
    { code : String
    , cardOptions : CardOptions
    , playedCards : Dict ClientId (Maybe Int)
    }


type alias FrontendModel =
    { game : Maybe Game
    , selectedCard : Maybe Int
    , enteredGameCode : String
    , cardOptions : CardOptions
    , hideStats : Bool
    , device : Device
    , key : Maybe Key
    }


type alias BackendModel =
    Dict String Game


type FrontendMsg
    = NoOpFrontendMsg
    | GameCreated
    | CardSelected Game Int
    | GameCodeEntered String
    | RequestGame
    | LeftGame
    | ToggleStats
    | ResetGame Game
    | ChooseCardOptions CardOptions
    | CopyCodeToClipboard Game
    | GotWindowDimensions Int Int


type ToBackend
    = NoOpToBackend
    | CreateGame CardOptions
    | JoinGame String
    | LeaveGame
    | UpdatePlayerCard String Int
    | ResetGameCards String


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | CodeCreated ClientId CardOptions String


type ToFrontend
    = NoOpToFrontend
    | CreatedGameReceived (Maybe Game)
    | GameReceived (Maybe Game)
    | GameReset (Maybe Game)
