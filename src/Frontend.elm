port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, hr)
import Html.Attributes as HtmlAttributes
import Icons
import Lamdera
import Maybe exposing (..)
import Stats exposing (mostCommon)
import Task
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
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Planning Poker"
                , body = [ view model ]
                }
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { game = Nothing
      , selectedCard = Nothing
      , enteredGameCode = ""
      , hideStats = True
      , cardOptions = fibonacci
      , device = { class = Desktop, orientation = Landscape }
      }
    , Task.attempt
        (\viewport ->
            case viewport of
                Err _ ->
                    NoOpFrontendMsg

                Ok vp ->
                    GotWindowDimensions (round vp.scene.width) (round vp.scene.height)
        )
        Browser.Dom.getViewport
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GameCreated ->
            ( model, Lamdera.sendToBackend (CreateGame model.cardOptions) )

        CardSelected game card ->
            ( { model | selectedCard = Just card }, Lamdera.sendToBackend (UpdatePlayerCard game.code card) )

        GameCodeEntered code ->
            ( { model | enteredGameCode = code }, Cmd.none )

        RequestGame ->
            ( model, Lamdera.sendToBackend (JoinGame model.enteredGameCode) )

        LeftGame ->
            ( { model | game = Nothing }, Lamdera.sendToBackend LeaveGame )

        ToggleStats ->
            ( { model | hideStats = not model.hideStats }, Cmd.none )

        ResetGame game ->
            ( { model | selectedCard = Nothing, hideStats = True }, Lamdera.sendToBackend (ResetGameCards game.code) )

        ChooseCardOptions options ->
            ( { model | cardOptions = options }, Cmd.none )

        CopyCodeToClipboard game ->
            ( model, copy_to_clipboard_to_js game.code )

        GotWindowDimensions w h ->
            ( { model | device = classifyDevice { width = w, height = h } }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        GameReceived maybeGame ->
            ( { model | game = maybeGame, enteredGameCode = "" }, Cmd.none )

        GameReset maybeGame ->
            ( { model | game = maybeGame, selectedCard = Nothing, hideStats = True }, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotWindowDimensions w h)


port copy_to_clipboard_to_js : String -> Cmd msg


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
        , centerX
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


getCards : Game -> List Int
getCards game =
    game.playedCards
        |> Dict.values
        |> List.filterMap identity


view : Model -> Html FrontendMsg
view model =
    layout []
        (case model.game of
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
                                { onPress = Just GameCreated
                                , label = text "Start New Game"
                                }
                            , el [ centerX ]
                                (Input.radio
                                    [ paddingXY 0 16
                                    ]
                                    { onChange = ChooseCardOptions
                                    , selected = Just model.cardOptions
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
                                { onChange = GameCodeEntered
                                , text = model.enteredGameCode
                                , placeholder = Just (Input.placeholder [ Font.color (rgb255 217 217 217) ] (text "Enter code"))
                                , label = Input.labelHidden "Enter a game code"
                                }
                            , Input.button
                                [ centerX, padding 16, Border.rounded 8, Background.color builtins.green, Font.color builtins.white ]
                                { onPress = Just RequestGame, label = text "Join Existing Game" }
                            ]
                        ]
                    ]

            Just game ->
                let
                    cards : List Int
                    cards =
                        getCards game

                    mostCommonCard : List Int
                    mostCommonCard =
                        mostCommon cards
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
                                { onPress = Just LeftGame
                                , label = row [ spacingXY 8 0 ] [ text "⬅", text "Leave Game" ]
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
                            [ row [ spacing 8 ] [ el [ alignRight ] (text ("Game Code: " ++ game.code)), Input.button [] { onPress = Just (CopyCodeToClipboard game), label = Icons.clipboard } ]
                            , el [ alignRight ] (text ("Connected Players: " ++ String.fromInt (Dict.size game.playedCards)))
                            ]
                        ]
                    , Element.row [ height (fillPortion 4), centerX, spacingXY 0 32, paddingXY 0 24 ]
                        [ (if model.device.class == Phone then
                            Input.radio

                           else
                            Input.radioRow
                          )
                            [ spacing 16, centerX, centerY ]
                            { onChange = CardSelected game
                            , selected = model.selectedCard
                            , label = Input.labelAbove [ centerX, paddingXY 0 16 ] (text "Select a Card")
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
                                    game.cardOptions
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
                                        { onPress = Just (ResetGame game)
                                        , label = text "Reset"
                                        }
                                    ]
                                ]
                                (if model.hideStats then
                                    [ statSection "Cards counted" (List.length cards |> String.fromInt) ]

                                 else
                                    [ statSection "Cards"
                                        (if List.isEmpty cards then
                                            "None"

                                         else
                                            cards
                                                |> List.sort
                                                |> List.map String.fromInt
                                                |> String.join ", "
                                        )
                                    , statSection "Most Common Card"
                                        (if List.isEmpty mostCommonCard then
                                            "None"

                                         else
                                            mostCommonCard
                                                |> List.map String.fromInt
                                                |> String.join ", "
                                        )
                                    , statSection "Average"
                                        (if List.isEmpty cards then
                                            "None"

                                         else
                                            (List.sum cards // List.length cards) |> String.fromInt
                                        )
                                    ]
                                )
                            )
                        ]
                    ]
        )



-- CONSTANTS


timesTwo : CardOptions
timesTwo =
    [ 1, 2, 4, 8, 16, 24, 36, 48, 72 ]


fibonacci : CardOptions
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
