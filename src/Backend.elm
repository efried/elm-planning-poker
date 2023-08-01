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
    ( { rooms = Dict.empty }
    , Cmd.none
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
                updatedRooms : Dict.Dict String Room
                updatedRooms =
                    Dict.map
                        (\key room ->
                            { key = key
                            , points =
                                List.filter
                                    (\point -> point.clientId /= clientId)
                                    room.points
                            }
                        )
                        model.rooms

                clientRoomKeys : List String
                clientRoomKeys =
                    model.rooms
                        |> Dict.filter
                            (\_ room -> List.any (\point -> point.clientId == clientId) room.points)
                        |> Dict.keys

                batchUpdates : String -> List (Cmd backendMsg) -> List (Cmd backendMsg)
                batchUpdates key updates =
                    Dict.get key updatedRooms
                        |> Maybe.map
                            (\room ->
                                List.map
                                    (\point ->
                                        sendToFrontend point.clientId (PlanningRoomReceived (Just room))
                                    )
                                    room.points
                            )
                        |> Maybe.withDefault []
                        |> List.append updates
            in
            ( { rooms = updatedRooms
              }
            , Cmd.batch
                (List.foldl batchUpdates [] clientRoomKeys)
            )

        KeyCreated clientId key ->
            let
                room : Room
                room =
                    { key = key, points = [ { clientId = clientId, value = Nothing } ] }
            in
            ( { rooms = Dict.insert key room model.rooms }
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
                mappedRooms : Dict.Dict String Room
                mappedRooms =
                    Dict.update key
                        (Maybe.map
                            (\room ->
                                { room | points = { clientId = clientId, value = Nothing } :: room.points }
                            )
                        )
                        model.rooms

                updatedRoom : Maybe Room
                updatedRoom =
                    Dict.get key mappedRooms

                clientsToUpdate : List ClientId
                clientsToUpdate =
                    updatedRoom
                        |> Maybe.map (\room -> List.map (\point -> point.clientId) room.points)
                        |> Maybe.withDefault []
            in
            ( { model | rooms = mappedRooms }
            , Cmd.batch
                (List.map
                    (\client ->
                        sendToFrontend client (PlanningRoomReceived updatedRoom)
                    )
                    clientsToUpdate
                )
            )

        LeavePlanningRoom ->
            update (ClientDisconnected sessionId clientId) model


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
