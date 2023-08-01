module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Lamdera
import Maybe exposing (..)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ -> \_ -> init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view =
            \model ->
                { title = "Planning Poker"
                , body = [ view model ]
                }
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { room = Nothing, scoreSelection = One, roomCode = "" }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PlanningRoomCreated ->
            ( model, Lamdera.sendToBackend CreatePlanningRoom )

        ScoreSelected score ->
            ( { model | scoreSelection = score }, Cmd.none )

        RoomCodeEntered code ->
            ( { model | roomCode = code }, Cmd.none )

        RequestPlanningRoom ->
            ( model, Lamdera.sendToBackend (JoinPlanningRoom model.roomCode) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        PlanningRoomReceived maybeRoom ->
            ( { model | room = maybeRoom, roomCode = "" }, Cmd.none )


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
                                , text = model.roomCode
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
                column [ centerX, centerY, spacing 16 ]
                    [ el [ centerX ] (text ("Key: " ++ room.key))
                    , el [ centerX ] (text ("Connected clients: " ++ String.fromInt (List.length room.points)))
                    , Input.radioRow [ padding 10, spacing 20, centerX, centerY ]
                        { onChange = ScoreSelected
                        , selected = Just model.scoreSelection
                        , label = Input.labelAbove [ centerX ] (text "Select a score")
                        , options =
                            List.map
                                (\val ->
                                    Input.optionWith val
                                        (radioOption
                                            (text
                                                (val
                                                    |> scoreToInt
                                                    |> String.fromInt
                                                )
                                            )
                                        )
                                )
                                [ One, Two, Four, Eight, Sixteen ]
                        }
                    ]
        )



-- COLORS


type alias BuiltInColors =
    { babyBlue : Color
    , yellow : Color
    , red : Color
    , pink : Color
    , green : Color
    , white : Color
    , black : Color
    }


builtins : BuiltInColors
builtins =
    { babyBlue = rgb255 138 205 234
    , yellow = rgb255 242 255 73
    , red = rgb255 146 45 80
    , pink = rgb255 205 172 161
    , green = rgb255 16 69 71
    , white = rgb255 255 255 255
    , black = rgb255 0 0 0
    }
