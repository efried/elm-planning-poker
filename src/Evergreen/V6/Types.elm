module Evergreen.V6.Types exposing (..)

import Dict
import Element
import Lamdera


type alias CardOptions =
    List Int


type alias Game =
    { code : String
    , cardOptions : CardOptions
    , playedCards : Dict.Dict Lamdera.ClientId (Maybe Int)
    }


type alias FrontendModel =
    { game : Maybe Game
    , selectedCard : Maybe Int
    , enteredGameCode : String
    , cardOptions : CardOptions
    , hideStats : Bool
    , device : Element.Device
    }


type alias BackendModel =
    Dict.Dict String Game


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
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | CodeCreated Lamdera.ClientId CardOptions String


type ToFrontend
    = NoOpToFrontend
    | GameReceived (Maybe Game)
