module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Lamdera
import Maybe exposing (..)
import Stats exposing (mode)
import Types exposing (..)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ -> \_ -> init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view =
            \model ->
                { title = "Planning Poker"
                , body = [ view model ]
                }
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { room = Nothing, scoreSelection = Nothing, enteredRoomCode = "", hideStats = True }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PlanningRoomCreated ->
            ( model, Lamdera.sendToBackend CreatePlanningRoom )

        ScoreSelected room score ->
            ( { model | scoreSelection = Just score }, Lamdera.sendToBackend (UpdateClientScore room.key score) )

        RoomCodeEntered code ->
            ( { model | enteredRoomCode = code }, Cmd.none )

        RequestPlanningRoom ->
            ( model, Lamdera.sendToBackend (JoinPlanningRoom model.enteredRoomCode) )

        LeftPlanningRoom ->
            ( { model | room = Nothing }, Lamdera.sendToBackend LeavePlanningRoom )

        ToggleStats ->
            ( { model | hideStats = not model.hideStats }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        PlanningRoomReceived maybeRoom ->
            ( { model | room = maybeRoom, enteredRoomCode = "" }, Cmd.none )


radioOption : Element msg -> Input.OptionState -> Element.Element msg
radioOption optionLabel status =
    let
        glowSize =
            case status of
                Input.Selected ->
                    4

                _ ->
                    0

        boxLength =
            case status of
                Input.Selected ->
                    px 52

                _ ->
                    px 48
    in
    el
        [ height boxLength
        , width boxLength
        , padding 8
        , Border.rounded 4
        , Border.glow builtins.babyBlue glowSize
        , Background.color builtins.green
        ]
        (el
            [ centerX
            , centerY
            , Font.color builtins.white
            ]
            optionLabel
        )


statSection : String -> String -> Element.Element msg
statSection label value =
    row [ width fill, spaceEvenly ]
        [ el [ Font.family [ Font.monospace ] ] (text label)
        , text value
        ]


getScores : Room -> List Int
getScores room =
    room.points
        |> Dict.values
        |> List.filterMap identity


view : Model -> Html FrontendMsg
view model =
    layout []
        (case model.room of
            Nothing ->
                column [ width fill, height fill ]
                    [ row
                        [ centerX
                        , height (fillPortion 3)
                        , width fill
                        , padding 16
                        ]
                        [ Input.button
                            [ centerX
                            , padding 16
                            , Border.rounded 8
                            , Background.color builtins.green
                            , Font.color builtins.white
                            , alignBottom
                            ]
                            { onPress = Just PlanningRoomCreated
                            , label = text "Start New Session"
                            }
                        ]
                    , row [ height (fillPortion 1), centerX ] [ text "OR" ]
                    , row
                        [ height (fillPortion 3), width fill, padding 16 ]
                        [ column [ centerX, alignTop, spacing 16 ]
                            [ Input.text [ width fill, Font.center ]
                                { onChange = RoomCodeEntered
                                , text = model.enteredRoomCode
                                , placeholder = Just (Input.placeholder [ Font.color (rgb255 217 217 217) ] (text "Enter code"))
                                , label = Input.labelHidden "Enter a room code"
                                }
                            , Input.button
                                [ centerX, padding 16, Border.rounded 8, Background.color builtins.green, Font.color builtins.white ]
                                { onPress = Just RequestPlanningRoom, label = text "Join an existing room" }
                            ]
                        ]
                    ]

            Just room ->
                let
                    scores : List Int
                    scores =
                        getScores room

                    roomMode : List Int
                    roomMode =
                        mode scores
                in
                column [ width fill, height fill, padding 16 ]
                    [ row [ width fill ]
                        [ column [ alignLeft ]
                            [ Input.button
                                [ centerX
                                , padding 8
                                , Border.rounded 8
                                , Border.color builtins.green
                                , Border.width 2
                                , Font.color builtins.green
                                , Font.size 16
                                , alignBottom
                                ]
                                { onPress = Just LeftPlanningRoom
                                , label = row [ spacingXY 8 0 ] [ text "⬅", text "Leave" ]
                                }
                            ]
                        , column
                            [ spacing 8
                            , alignRight
                            , Font.size 16
                            , Font.color builtins.green
                            , Font.semiBold
                            , Font.family [ Font.monospace ]
                            ]
                            [ text ("Room key: " ++ room.key)
                            , text ("Connected clients: " ++ String.fromInt (Dict.size room.points))
                            ]
                        ]
                    , row [ height (fillPortion 4), centerX, spacingXY 0 32 ]
                        [ Input.radioRow [ spacing 16, centerX, centerY ]
                            { onChange = ScoreSelected room
                            , selected = model.scoreSelection
                            , label = Input.labelAbove [ centerX, paddingXY 0 16 ] (text "Select a score")
                            , options =
                                List.map
                                    (\val ->
                                        Input.optionWith val
                                            (val
                                                |> String.fromInt
                                                |> text
                                                |> radioOption
                                            )
                                    )
                                    scoreOptions
                            }
                        ]
                    , row
                        [ centerX, height (fillPortion 2), width fill ]
                        [ column [ alignTop, centerX, width (fill |> maximum 400), spacing 16 ]
                            (List.append
                                [ Input.button
                                    [ centerX
                                    , padding 8
                                    , Font.color builtins.green
                                    , Border.color builtins.green
                                    , Border.rounded 8
                                    , Border.width 2
                                    ]
                                    { onPress = Just ToggleStats
                                    , label =
                                        if model.hideStats then
                                            text "Reveal Results"

                                        else
                                            text "Hide Results"
                                    }
                                ]
                                (if model.hideStats then
                                    [ statSection "Votes counted" (List.length scores |> String.fromInt) ]

                                 else
                                    [ statSection "Scores"
                                        (if List.isEmpty scores then
                                            "None"

                                         else
                                            scores
                                                |> List.sort
                                                |> List.map String.fromInt
                                                |> String.join ", "
                                        )
                                    , statSection "Mode"
                                        (if List.isEmpty roomMode then
                                            "None"

                                         else
                                            roomMode
                                                |> List.map String.fromInt
                                                |> String.join ", "
                                        )
                                    , statSection "Average"
                                        (if List.isEmpty scores then
                                            "None"

                                         else
                                            (List.sum scores // List.length scores) |> String.fromInt
                                        )
                                    ]
                                )
                            )
                        ]
                    ]
        )



-- CONSTANTS


scoreOptions : List Int
scoreOptions =
    [ 1, 2, 4, 8, 16, 24, 36, 48, 72 ]


type alias BuiltInColors =
    { babyBlue : Color
    , green : Color
    , white : Color
    , black : Color
    }


builtins : BuiltInColors
builtins =
    { babyBlue = rgb255 138 205 234
    , green = rgb255 16 69 71
    , white = rgb255 255 255 255
    , black = rgb255 0 0 0
    }
