module Types exposing (BackendModel, BackendMsg(..), CardOptions, FrontendModel, FrontendMsg(..), Game, GameId, GameWithSessions, ToBackend(..), ToFrontend(..))

import Dict exposing (Dict)
import Element exposing (Device)
import Lamdera exposing (ClientId, Key, SessionId)


type alias CardOptions =
    List Int


type alias GameId =
    String


type alias GameWithSessions =
    { code : GameId
    , cardOptions : CardOptions
    , playedCards : Dict SessionId (Maybe Int)
    }


type alias Game =
    { code : GameId
    , cardOptions : CardOptions
    , playedCards : List (Maybe Int)
    }


type alias FrontendModel =
    { game : Maybe Game
    , existingGame : Maybe GameId
    , selectedCard : Maybe Int
    , enteredGameCode : String
    , cardOptions : CardOptions
    , hideStats : Bool
    , device : Device
    , key : Maybe Key
    }


type alias BackendModel =
    { sessions : Dict SessionId (Maybe GameId)
    , games : Dict GameId GameWithSessions
    }


type FrontendMsg
    = NoOpFrontendMsg
    | GameCreated
    | CardSelected Game Int
    | GameCodeEntered String
    | RequestGame
    | RejoinGame GameId
    | LeftGame GameId
    | ToggleStats
    | ResetGame Game
    | ChooseCardOptions CardOptions
    | CopyCodeToClipboard Game
    | GotWindowDimensions Int Int


type ToBackend
    = NoOpToBackend
    | CreateGame CardOptions
    | JoinGame GameId
    | CheckForSession
    | LeaveGame GameId
    | UpdatePlayerCard GameId Int
    | ResetGameCards GameId


type BackendMsg
    = NoOpConnectionMsg SessionId ClientId
    | CodeCreated SessionId CardOptions GameId
    | NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | CreatedGameReceived (Maybe Game)
    | GameReceived (Maybe Game)
    | ExistingGameReceived GameId
    | GameReset (Maybe Game)
