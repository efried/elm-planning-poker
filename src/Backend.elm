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


allRoomClientUpdates : Maybe Room -> List (Cmd BackendMsg)
allRoomClientUpdates room =
    room
        |> Maybe.map (\r -> Dict.keys r.points)
        |> Maybe.withDefault []
        |> List.map
            (\client ->
                sendToFrontend client (PlanningRoomReceived room)
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
                clientRooms : Dict.Dict String Room
                clientRooms =
                    model
                        |> Dict.filter (\_ room -> Dict.member clientId room.points)
                        |> Dict.map
                            (\key room ->
                                { key = key
                                , points = Dict.remove clientId room.points
                                }
                            )

                nonEmptyRooms : Dict.Dict String Room
                nonEmptyRooms =
                    Dict.union
                        (Dict.filter (\_ room -> Dict.isEmpty room.points |> not) clientRooms)
                        (Dict.diff model clientRooms)
            in
            ( nonEmptyRooms
            , Cmd.batch
                (List.foldl (Just >> allRoomClientUpdates >> List.append)
                    []
                    (Dict.values clientRooms)
                )
            )

        KeyCreated clientId key ->
            let
                room : Room
                room =
                    { key = key, points = Dict.singleton clientId Nothing }
            in
            ( Dict.insert key room model
            , sendToFrontend clientId (PlanningRoomReceived (Just room))
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

        CreatePlanningRoom ->
            ( model, Random.generate (KeyCreated clientId) keyGenerator )

        JoinPlanningRoom key ->
            let
                updatedRooms : Dict.Dict String Room
                updatedRooms =
                    Dict.update key
                        (Maybe.map
                            (\room -> { room | points = Dict.insert clientId Nothing room.points })
                        )
                        model
            in
            ( updatedRooms
            , Cmd.batch (Dict.get key updatedRooms |> allRoomClientUpdates)
            )

        UpdateClientScore roomCode score ->
            let
                updatedRooms : Dict.Dict String Room
                updatedRooms =
                    Dict.update
                        roomCode
                        (Maybe.map
                            (\room ->
                                { room
                                    | points =
                                        Dict.update clientId (Maybe.map (\_ -> Just score)) room.points
                                }
                            )
                        )
                        model
            in
            ( updatedRooms
            , Cmd.batch (Dict.get roomCode updatedRooms |> allRoomClientUpdates)
            )

        LeavePlanningRoom ->
            update (ClientDisconnected sessionId clientId) model

        ResetRoomScores roomCode ->
            let
                updatedRooms : Dict.Dict String Room
                updatedRooms =
                    Dict.update
                        roomCode
                        (Maybe.map (\room -> { room | points = Dict.map (\_ _ -> Nothing) room.points }))
                        model
            in
            ( updatedRooms, Cmd.batch (Dict.get roomCode updatedRooms |> allRoomClientUpdates) )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
