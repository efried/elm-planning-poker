module Evergreen.V11.Types exposing (..)

import Dict
import Element
import Lamdera


type alias GameId =
    String


type alias CardOptions =
    List Int


type alias Game =
    { code : GameId
    , cardOptions : CardOptions
    , playedCards : List (Maybe Int)
    }


type alias FrontendModel =
    { game : Maybe Game
    , selectedCard : Maybe Int
    , enteredGameCode : String
    , cardOptions : CardOptions
    , hideStats : Bool
    , device : Element.Device
    , key : Maybe Lamdera.Key
    }


type alias GameWithSessions =
    { code : GameId
    , cardOptions : CardOptions
    , playedCards : Dict.Dict Lamdera.SessionId (Maybe Int)
    }


type alias BackendModel =
    { sessions : Dict.Dict Lamdera.SessionId (Maybe GameId)
    , games : Dict.Dict GameId GameWithSessions
    }


type FrontendMsg
    = NoOpFrontendMsg
    | GameCreated
    | CardSelected Game Int
    | GameCodeEntered String
    | RequestGame
    | LeftGame Game
    | ToggleStats
    | ResetGame Game
    | ChooseCardOptions CardOptions
    | CopyCodeToClipboard Game
    | GotWindowDimensions Int Int


type ToBackend
    = NoOpToBackend
    | CreateGame CardOptions
    | JoinGame GameId
    | LeaveGame GameId
    | UpdatePlayerCard GameId Int
    | ResetGameCards GameId


type BackendMsg
    = NoOpConnectionMsg Lamdera.SessionId Lamdera.ClientId
    | CodeCreated Lamdera.SessionId CardOptions GameId
    | NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | CreatedGameReceived (Maybe Game)
    | GameReceived (Maybe Game)
    | GameReset (Maybe Game)
