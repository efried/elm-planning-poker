module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, hr)
import Html.Attributes as HtmlAttributes
import Lamdera
import Maybe exposing (..)
import Stats exposing (mostCommon)
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
    ( { room = Nothing, scoreSelection = Nothing, enteredRoomCode = "", hideStats = True, pointOptions = fibonacci }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PlanningRoomCreated ->
            ( model, Lamdera.sendToBackend (CreatePlanningRoom model.pointOptions) )

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

        ResetRoom room ->
            ( { model | scoreSelection = Nothing }, Lamdera.sendToBackend (ResetRoomScores room.key) )

        ChoosePointOptions options ->
            ( { model | pointOptions = options }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        PlanningRoomReceived maybeRoom ->
            ( { model | room = maybeRoom, enteredRoomCode = "" }, Cmd.none )


logo : Element msg
logo =
    el
        [ centerX
        , Font.color builtins.green
        , Font.shadow
            { color = builtins.black
            , offset = ( 2, 2 )
            , blur = 1
            }
        , Font.heavy
        , Font.size 48
        ]
        (text "Planning Poker")


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
                        [ height (fillPortion 5)
                        , width fill
                        ]
                        [ column
                            [ height fill
                            , centerX
                            ]
                            [ el
                                [ centerX
                                , Font.color builtins.green
                                , Font.shadow
                                    { color = builtins.black
                                    , offset = ( 2, 2 )
                                    , blur = 1
                                    }
                                , Font.heavy
                                , Font.size 48
                                , paddingXY 0 80
                                ]
                                (text "Planning Poker")
                            , Input.button
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
                            , el [ centerX ]
                                (Input.radio
                                    [ paddingXY 0 16
                                    ]
                                    { onChange = ChoosePointOptions
                                    , selected = Just model.pointOptions
                                    , label =
                                        Input.labelLeft
                                            [ paddingEach { left = 0, right = 16, top = 0, bottom = 0 } ]
                                            (text "Options")
                                    , options =
                                        [ Input.option fibonacci (text "Fibonacci")
                                        , Input.option timesTwo (text "Multiply by 2")
                                        ]
                                    }
                                )
                            ]
                        ]
                    , row [ height (fillPortion 1), centerX, width (fill |> maximum 200) ]
                        [ html (hr [ HtmlAttributes.style "width" "100%" ] []) ]
                    , row
                        [ height (fillPortion 5), width fill, padding 16 ]
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

                    mostCommonScore : List Int
                    mostCommonScore =
                        mostCommon scores
                in
                column [ width fill, height fill, padding 16 ]
                    [ wrappedRow [ width fill ]
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
                            [ el [ alignRight ] (text ("Room key: " ++ room.key))
                            , el [ alignRight ] (text ("Connected clients: " ++ String.fromInt (Dict.size room.points)))
                            ]
                        ]
                    , Element.row [ height (fillPortion 4), centerX, spacingXY 0 32 ]
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
                                    room.pointOptions
                            }
                        ]
                    , row
                        [ centerX, height (fillPortion 2), width fill ]
                        [ column [ alignTop, centerX, width (fill |> maximum 400), spacing 16 ]
                            (List.append
                                [ row [ spaceEvenly, width fill ]
                                    [ Input.button
                                        [ padding 8
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
                                    , Input.button
                                        [ padding 8
                                        , Font.color builtins.red
                                        , Border.color builtins.red
                                        , Border.rounded 8
                                        , Border.width 2
                                        ]
                                        { onPress = Just (ResetRoom room)
                                        , label = text "Reset"
                                        }
                                    ]
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
                                    , statSection "Most Common Score"
                                        (if List.isEmpty mostCommonScore then
                                            "None"

                                         else
                                            mostCommonScore
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


timesTwo : PointOptions
timesTwo =
    [ 1, 2, 4, 8, 16, 24, 36, 48, 72 ]


fibonacci : PointOptions
fibonacci =
    [ 1, 2, 3, 5, 8, 13, 21, 34, 55 ]


type alias BuiltInColors =
    { babyBlue : Color
    , green : Color
    , white : Color
    , black : Color
    , red : Color
    }


builtins : BuiltInColors
builtins =
    { babyBlue = rgb255 138 205 234
    , green = rgb255 16 69 71
    , white = rgb255 255 255 255
    , black = rgb255 0 0 0
    , red = rgb255 222 108 131
    }
