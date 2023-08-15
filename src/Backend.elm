module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Random
import Random.Char
import Random.String as RandomString
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( Dict.empty, Cmd.none )


allPlayersUpdates : Maybe Game -> List (Cmd BackendMsg)
allPlayersUpdates maybeGame =
    maybeGame
        |> Maybe.map (\game -> Dict.keys game.playedCards)
        |> Maybe.withDefault []
        |> List.map
            (\player ->
                sendToFrontend player (GameReceived maybeGame)
            )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model
            , Cmd.batch
                [ broadcast NoOpToFrontend
                ]
            )

        ClientDisconnected sessionId clientId ->
            let
                playerGames : Dict.Dict String Game
                playerGames =
                    model
                        |> Dict.filter (\_ game -> Dict.member clientId game.playedCards)
                        |> Dict.map
                            (\_ game ->
                                { game
                                    | playedCards = Dict.remove clientId game.playedCards
                                }
                            )

                nonEmptyGames : Dict.Dict String Game
                nonEmptyGames =
                    Dict.union
                        (Dict.filter (\_ game -> Dict.isEmpty game.playedCards |> not) playerGames)
                        (Dict.diff model playerGames)
            in
            ( nonEmptyGames
            , Cmd.batch
                (List.foldl (Just >> allPlayersUpdates >> List.append)
                    []
                    (Dict.values playerGames)
                )
            )

        CodeCreated clientId pointOptions code ->
            let
                game : Game
                game =
                    { code = code, cardOptions = pointOptions, playedCards = Dict.singleton clientId Nothing }
            in
            ( Dict.insert code game model
            , sendToFrontend clientId (GameReceived (Just game))
            )

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
            ( model, Random.generate (CodeCreated clientId pointOptions) keyGenerator )

        JoinGame code ->
            let
                updatedGames : Dict.Dict String Game
                updatedGames =
                    Dict.update code
                        (Maybe.map
                            (\game -> { game | playedCards = Dict.insert clientId Nothing game.playedCards })
                        )
                        model
            in
            ( updatedGames
            , Cmd.batch (Dict.get code updatedGames |> allPlayersUpdates)
            )

        UpdatePlayerCard gameCode card ->
            let
                updatedGames : Dict.Dict String Game
                updatedGames =
                    Dict.update
                        gameCode
                        (Maybe.map
                            (\game ->
                                { game
                                    | playedCards =
                                        Dict.update clientId (Maybe.map (\_ -> Just card)) game.playedCards
                                }
                            )
                        )
                        model
            in
            ( updatedGames
            , Cmd.batch (Dict.get gameCode updatedGames |> allPlayersUpdates)
            )

        LeaveGame ->
            update (ClientDisconnected sessionId clientId) model

        ResetGameCards gameCode ->
            let
                updatedGames : Dict.Dict String Game
                updatedGames =
                    Dict.update
                        gameCode
                        (Maybe.map (\game -> { game | playedCards = Dict.map (\_ _ -> Nothing) game.playedCards }))
                        model
            in
            ( updatedGames, Cmd.batch (Dict.get gameCode updatedGames |> allPlayersUpdates) )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
