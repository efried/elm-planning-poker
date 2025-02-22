module Backend exposing (Model, app, init, subscriptions, update, updateFromFrontend)

import Dict
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Random
import Random.Char
import Random.String as RandomString
import Types exposing (..)


type alias Model =
    BackendModel


app :
    { init : ( Model, Cmd BackendMsg )
    , update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
    , subscriptions : Model -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty, games = Dict.empty }, Cmd.none )


allPlayersUpdates : Maybe GameWithSessions -> List (Cmd BackendMsg)
allPlayersUpdates maybeGame =
    maybeGame
        |> Maybe.map (\game -> Dict.keys game.playedCards)
        |> Maybe.withDefault []
        |> List.map
            (\player ->
                sendToFrontend player (GameReceived (maybeGame |> Maybe.map gameForFrontend))
            )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        CodeCreated sessionId pointOptions code ->
            let
                game : GameWithSessions
                game =
                    { code = code, cardOptions = pointOptions, playedCards = Dict.singleton sessionId Nothing }
            in
            ( { sessions = Dict.update sessionId (\_ -> Just (Just code)) model.sessions
              , games = Dict.insert code game model.games
              }
            , sendToFrontend sessionId (CreatedGameReceived (Just (gameForFrontend game)))
            )

        NoOpConnectionMsg sessionId clientId ->
            ( model, Cmd.none )

        NoOpBackendMsg ->
            ( model, Cmd.none )


keyGenerator : Random.Generator String
keyGenerator =
    RandomString.string 20 Random.Char.english


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        CreateGame pointOptions ->
            ( model, Random.generate (CodeCreated sessionId pointOptions) keyGenerator )

        JoinGame code ->
            if Dict.member code model.games then
                let
                    updatedGames =
                        Dict.update code
                            (Maybe.map
                                (\game -> { game | playedCards = Dict.insert sessionId Nothing game.playedCards })
                            )
                            model.games
                in
                ( { games = updatedGames
                  , sessions = Dict.update sessionId (\_ -> Just (Just code)) model.sessions
                  }
                , Cmd.batch (Dict.get code updatedGames |> allPlayersUpdates)
                )

            else
                ( model, sendToFrontend sessionId (GameReceived Nothing) )

        UpdatePlayerCard gameId card ->
            let
                updatedGames =
                    Dict.update
                        gameId
                        (Maybe.map
                            (\game ->
                                { game
                                    | playedCards =
                                        Dict.update sessionId (Maybe.map (\_ -> Just card)) game.playedCards
                                }
                            )
                        )
                        model.games
            in
            ( { model | games = updatedGames }
            , Cmd.batch (Dict.get gameId updatedGames |> allPlayersUpdates)
            )

        LeaveGame gameId ->
            let
                updatedGames =
                    Dict.update
                        gameId
                        (Maybe.map
                            (\game ->
                                { game
                                    | playedCards =
                                        Dict.remove sessionId game.playedCards
                                }
                            )
                        )
                        model.games

                gamePlayerCount =
                    Dict.get gameId model.games
                        |> Maybe.map .playedCards
                        |> Maybe.map Dict.size
                        |> Maybe.withDefault 0
            in
            ( { games =
                    if gamePlayerCount > 0 then
                        updatedGames

                    else
                        Dict.remove gameId model.games
              , sessions = Dict.update sessionId (\_ -> Nothing) model.sessions
              }
            , Cmd.batch
                (sendToFrontend sessionId (GameReceived Nothing)
                    :: (Dict.get gameId updatedGames |> allPlayersUpdates)
                )
            )

        ResetGameCards gameCode ->
            let
                updatedGames : Dict.Dict GameId GameWithSessions
                updatedGames =
                    Dict.update
                        gameCode
                        (Maybe.map (\game -> { game | playedCards = Dict.map (\_ _ -> Nothing) game.playedCards }))
                        model.games

                resetGame : Maybe GameWithSessions
                resetGame =
                    Dict.get gameCode updatedGames
            in
            ( { model | games = updatedGames }
            , Cmd.batch
                (resetGame
                    |> Maybe.map (\game -> Dict.keys game.playedCards)
                    |> Maybe.withDefault []
                    |> List.map
                        (\player ->
                            sendToFrontend player (GameReset (resetGame |> Maybe.map gameForFrontend))
                        )
                )
            )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect NoOpConnectionMsg
        , Lamdera.onDisconnect NoOpConnectionMsg
        ]



-- UTILS


gameForFrontend : GameWithSessions -> Game
gameForFrontend { code, cardOptions, playedCards } =
    Game code cardOptions (Dict.values playedCards)
