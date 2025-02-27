port module Frontend exposing (Model, app, builtins, copy_to_clipboard_to_js, init, subscriptions, update, updateFromBackend, view)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (pushUrl, replaceUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Icons
import Lamdera exposing (Key, Url)
import Maybe
import Stats exposing (mostCommon)
import Task
import Types exposing (..)
import Url.Builder
import Url.Parser


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = onUrlRequest
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


init : Url -> Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { game = Nothing
      , selectedCard = Nothing
      , enteredGameCode = ""
      , hideStats = True
      , cardOptions = fibonacci
      , device = { class = Desktop, orientation = Landscape }
      , key = Just key
      }
    , Cmd.batch
        [ Task.attempt
            (\viewport ->
                case viewport of
                    Err _ ->
                        NoOpFrontendMsg

                    Ok vp ->
                        GotWindowDimensions (round vp.scene.width) (round vp.scene.height)
            )
            Browser.Dom.getViewport
        , case Url.Parser.parse Url.Parser.string url of
            Just code ->
                Lamdera.sendToBackend (JoinGame code)

            Nothing ->
                Cmd.none
        ]
    )


onUrlRequest : Browser.UrlRequest -> FrontendMsg
onUrlRequest urlReqest =
    -- based on url either leave game or leave game and join new game
    NoOpFrontendMsg


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GameCreated ->
            ( model, Lamdera.sendToBackend (CreateGame model.cardOptions) )

        CardSelected game card ->
            ( { model | selectedCard = Maybe.Just card }, Lamdera.sendToBackend (UpdatePlayerCard game.code card) )

        GameCodeEntered code ->
            ( { model | enteredGameCode = code }, Cmd.none )

        RequestGame ->
            ( model, Lamdera.sendToBackend (JoinGame model.enteredGameCode) )

        LeftGame game ->
            ( { model | game = Nothing }
            , Cmd.batch
                [ case model.key of
                    Just key ->
                        pushUrl key (Url.Builder.absolute [] [])

                    Nothing ->
                        Cmd.none
                , Lamdera.sendToBackend (LeaveGame game.code)
                ]
            )

        ToggleStats ->
            ( { model | hideStats = not model.hideStats }, Cmd.none )

        ResetGame game ->
            ( { model | selectedCard = Nothing, hideStats = True }, Lamdera.sendToBackend (ResetGameCards game.code) )

        ChooseCardOptions options ->
            ( { model | cardOptions = options }, Cmd.none )

        CopyCodeToClipboard game ->
            let
                roomLink =
                    Url.Builder.absolute [ game.code ] []
            in
            ( model, copy_to_clipboard_to_js roomLink )

        GotWindowDimensions w h ->
            ( { model | device = classifyDevice { width = w, height = h } }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        CreatedGameReceived maybeGame ->
            ( { model | game = maybeGame, enteredGameCode = "" }
            , case ( maybeGame, model.key ) of
                ( Just game, Just key ) ->
                    pushUrl key (Url.Builder.absolute [ game.code ] [])

                _ ->
                    Cmd.none
            )

        GameReceived maybeGame ->
            ( { model | game = maybeGame, enteredGameCode = "" }
            , case ( maybeGame, model.key ) of
                ( Just game, Just key ) ->
                    replaceUrl key (Url.Builder.absolute [ game.code ] [])

                ( Nothing, Just key ) ->
                    pushUrl key (Url.Builder.absolute [] [])

                _ ->
                    Cmd.none
            )

        GameReset maybeGame ->
            ( { model | game = maybeGame, selectedCard = Nothing, hideStats = True }, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotWindowDimensions w h)


port copy_to_clipboard_to_js : String -> Cmd msg


radioOption : Element msg -> Input.OptionState -> Element.Element msg
radioOption optionLabel status =
    let
        glowSize : Float
        glowSize =
            case status of
                Input.Selected ->
                    4

                _ ->
                    0

        boxLength : Element.Length
        boxLength =
            case status of
                Input.Selected ->
                    px 76

                _ ->
                    px 72
    in
    el
        [ height boxLength
        , width boxLength
        , centerX
        , padding 8
        , Border.glow builtins.mint glowSize
        , Border.color builtins.mint
        , Border.width 2
        ]
        (el
            [ centerX
            , centerY
            , Font.color builtins.mintCream
            ]
            optionLabel
        )


statSection : String -> String -> Element.Element msg
statSection label value =
    row [ width fill, spaceEvenly, Font.family [ Font.monospace ], Font.color builtins.mintCream ]
        [ text label
        , text value
        ]


getCards : Game -> List Int
getCards game =
    game.playedCards
        |> List.filterMap identity


view : Model -> Html.Html FrontendMsg
view model =
    layout
        [ Font.family [ Font.typeface "Monaco", Font.typeface "Arial" ]
        , height fill
        , Background.gradient { angle = 0, steps = [ builtins.slate, builtins.black ] }
        ]
        (case model.game of
            Nothing ->
                column [ width fill, height fill ]
                    [ row [ height (fillPortion 3), width fill ]
                        [ column [ centerX, centerY ]
                            [ el
                                [ Font.color builtins.mint
                                , Font.extraBold
                                , Font.size 64
                                ]
                                (text "Planning Poker")
                            , Element.image [ centerX ] { src = "/header.svg", description = "Fanned out playing cards" }
                            ]
                        ]
                    , row [ height (fillPortion 2), width fill ]
                        [ column [ centerX, centerY ]
                            [ Input.button
                                [ centerX
                                , padding 16
                                , Border.color builtins.mint
                                , Border.width 2
                                , Font.color builtins.mint
                                , alignBottom
                                ]
                                { onPress = Maybe.Just GameCreated
                                , label = text "Start New Game"
                                }
                            , el [ centerX ]
                                (Input.radio
                                    [ paddingXY 0 16
                                    , Font.color builtins.mint
                                    ]
                                    { onChange = ChooseCardOptions
                                    , selected = Maybe.Just model.cardOptions
                                    , label = Input.labelHidden "Card Pattern"
                                    , options =
                                        [ Input.option fibonacci (text "Fibonacci")
                                        , Input.option timesTwo (text "Multiply by 2")
                                        ]
                                    }
                                )
                            ]
                        ]
                    ]

            Maybe.Just game ->
                let
                    cards : List Int
                    cards =
                        getCards game
                in
                column [ width fill, height fill, padding 16 ]
                    [ wrappedRow [ width fill ]
                        [ column [ alignLeft ]
                            [ Input.button
                                [ centerX
                                , padding 8
                                , Border.color builtins.mint
                                , Border.width 2
                                , Font.color builtins.mint
                                , Font.size 16
                                , alignBottom
                                ]
                                { onPress = Maybe.Just (LeftGame game)
                                , label = row [ spacingXY 8 0 ] [ text "⬅", text "Leave Game" ]
                                }
                            ]
                        , column
                            [ spacing 8
                            , alignRight
                            , Font.size 16
                            , Font.color builtins.mintCream
                            , Font.semiBold
                            , Font.family [ Font.monospace ]
                            ]
                            [ el [ alignRight ] (text ("Connected Players: " ++ String.fromInt (List.length game.playedCards)))
                            , Input.button [ alignRight ]
                                { onPress = Maybe.Just (CopyCodeToClipboard game)
                                , label =
                                    row
                                        [ spacing 8 ]
                                        [ el [ alignRight ] (text "Copy Game Link ")
                                        , Icons.copy
                                        ]
                                }
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
                            , label = Input.labelAbove [ Font.size 24, centerX, paddingXY 0 16, Font.color builtins.mint ] (text "Select a Card")
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
                                        , Font.color builtins.mint
                                        , Border.color builtins.mint
                                        , Border.width 2
                                        ]
                                        { onPress = Maybe.Just ToggleStats
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
                                        , Border.width 2
                                        ]
                                        { onPress = Maybe.Just (ResetGame game)
                                        , label = text "Reset"
                                        }
                                    ]
                                ]
                                (if model.hideStats then
                                    [ statSection "Cards counted" (List.length cards |> String.fromInt) ]

                                 else
                                    let
                                        mostCommonCard : List Int
                                        mostCommonCard =
                                            mostCommon cards
                                    in
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


builtins : { black : Color, red : Color, mintCream : Color, mint : Color, slate : Color }
builtins =
    { black = rgb255 37 37 37
    , red = rgb255 247 4 52
    , mintCream = rgb255 238 249 241
    , mint = rgb255 32 214 165
    , slate = rgb255 42 71 71
    }
