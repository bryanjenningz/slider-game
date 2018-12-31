port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (attribute, class, step, style, type_, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder, Value)
import Random



---- MODEL ----


type Popup
    = NotShown
    | ResultsPopup
    | InfoPopup
    | OrangeTokenPopup
    | ClearDataPopup


type alias Model =
    { slider : Float
    , target : Int
    , score : Int
    , orangeTokens : Int
    , closenessHistory : List Closeness
    , level : Int
    , popup : Popup
    , isSliderMouseDown : Bool
    , isMuted : Bool
    }


defaultModel : Model
defaultModel =
    { slider = 50
    , target = 0
    , score = 0
    , orangeTokens = 0
    , closenessHistory = []
    , level = 1
    , popup = NotShown
    , isSliderMouseDown = False
    , isMuted = False
    }


init : Value -> ( Model, Cmd Msg )
init savedDataValue =
    let
        maybeSavedData =
            Decode.decodeValue Decode.string savedDataValue
                |> Result.andThen (Decode.decodeString savedDataDecoder)
                |> Result.toMaybe
    in
    case maybeSavedData of
        Just savedData ->
            ( { defaultModel
                | target = savedData.target
                , score = savedData.score
                , orangeTokens = savedData.orangeTokens
                , level = savedData.level
              }
            , Cmd.none
            )

        Nothing ->
            ( defaultModel, generateRandomTarget )


type alias SavedData =
    { score : Int
    , orangeTokens : Int
    , level : Int
    , target : Int
    }


savedDataDecoder : Decoder SavedData
savedDataDecoder =
    Decode.map4 SavedData
        (Decode.field "score" Decode.int)
        (Decode.field "orangeTokens" Decode.int)
        (Decode.field "level" Decode.int)
        (Decode.field "target" Decode.int)


updateAndSaveData : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateAndSaveData ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , saveData
            { score = model.score
            , orangeTokens = model.orangeTokens
            , level = model.level
            , target = model.target
            }
        ]
    )


port saveData : SavedData -> Cmd msg


port playSound : String -> Cmd msg


closenessToString : Closeness -> String
closenessToString closeness =
    case closeness of
        TriplePerfect ->
            "triple-perfect"

        DoublePerfect ->
            "double-perfect"

        Perfect ->
            "perfect"

        SuperClose ->
            "super-close"

        Close ->
            "close"

        Far ->
            "far"

        SuperFar ->
            "super-far"


playClosenessSound : Closeness -> Cmd Msg
playClosenessSound closeness =
    playSound (closenessToString closeness ++ ".m4a")


generateRandomTarget : Cmd Msg
generateRandomTarget =
    Random.generate SetTarget (Random.int 0 100)


sliderValueDecoder : Decoder Float
sliderValueDecoder =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.map (String.toFloat >> Maybe.withDefault 0)


onSliderChange : (Float -> msg) -> Attribute msg
onSliderChange toMsg =
    on "change" (Decode.map toMsg sliderValueDecoder)



---- UPDATE ----


type Msg
    = SetTarget Int
    | SetSlider Float
    | ShowResults
    | NextLevel
    | Cry
    | ShowInfo
    | ClosePopup
    | ShowOrangeTokenPopup
    | ClearOrangeTokens
    | ShowClearDataPopup
    | ClearAllData
    | ToggleSound


type Closeness
    = TriplePerfect
    | DoublePerfect
    | Perfect
    | SuperClose
    | Close
    | Far
    | SuperFar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTarget newTarget ->
            ( { model | target = newTarget }
            , saveData
                { level = model.level
                , score = model.score
                , orangeTokens = model.orangeTokens
                , target = newTarget
                }
            )

        SetSlider slider ->
            ( { model | slider = slider }, Cmd.none )

        ShowResults ->
            let
                { closeness } =
                    getPointsAndCloseness model
            in
            ( { model | popup = ResultsPopup }
            , if model.isMuted then
                Cmd.none

              else
                playClosenessSound closeness
            )

        NextLevel ->
            let
                { points, closeness } =
                    getPointsAndCloseness model
            in
            ( { model
                | slider = 50
                , level = model.level + 1
                , popup = NotShown
                , score = model.score + points
                , orangeTokens =
                    model.orangeTokens
                        + (case closeness of
                            TriplePerfect ->
                                1

                            _ ->
                                0
                          )
                , closenessHistory = closeness :: model.closenessHistory
              }
            , generateRandomTarget
            )

        Cry ->
            ( model
            , if model.isMuted then
                Cmd.none

              else
                playSound "cry.m4a"
            )

        ShowInfo ->
            ( { model | popup = InfoPopup }, Cmd.none )

        ClosePopup ->
            ( { model | popup = NotShown }, Cmd.none )

        ShowOrangeTokenPopup ->
            ( { model | popup = OrangeTokenPopup }, Cmd.none )

        ClearOrangeTokens ->
            updateAndSaveData
                ( { model | orangeTokens = 0, popup = NotShown }, Cmd.none )

        ShowClearDataPopup ->
            ( { model | popup = ClearDataPopup }, Cmd.none )

        ClearAllData ->
            ( { defaultModel | isMuted = model.isMuted }, generateRandomTarget )

        ToggleSound ->
            ( { model | isMuted = not model.isMuted }, Cmd.none )



---- VIEW ----


getPointsAndCloseness : Model -> { points : Int, closeness : Closeness }
getPointsAndCloseness model =
    let
        sliderDifference =
            abs (round model.slider - model.target)

        ( points, closeness ) =
            if sliderDifference == 0 then
                case model.closenessHistory of
                    DoublePerfect :: _ ->
                        ( 1000, TriplePerfect )

                    Perfect :: _ ->
                        ( 400, DoublePerfect )

                    _ ->
                        ( 200, Perfect )

            else if sliderDifference <= 2 then
                ( 100, SuperClose )

            else if sliderDifference <= 5 then
                ( 100 - sliderDifference, Close )

            else if sliderDifference <= 10 then
                ( 1, Far )

            else
                ( 0, SuperFar )
    in
    { points = points, closeness = closeness }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "top-bar" ]
            [ div [ onClick ShowClearDataPopup ] [ text ("Level: " ++ String.fromInt model.level) ]
            , div [ onClick ShowClearDataPopup ] [ text ("Score: " ++ String.fromInt model.score) ]
            , div [ onClick ShowOrangeTokenPopup ] [ text ("🍊: " ++ String.fromInt model.orangeTokens) ]
            , div [ onClick ShowInfo ] [ div [ class "top-bar__info" ] [ text "?" ] ]
            ]
        , div [ class "target-info" ] [ text ("Target: " ++ String.fromInt model.target) ]
        , input
            [ type_ "range"
            , value (String.fromFloat model.slider)
            , onSliderChange SetSlider
            , attribute "aria-label" "game slider"
            , step "0.01"
            ]
            []
        , div [ onClick ShowResults, class "show-results__button" ] [ text "GO!" ]
        , case model.popup of
            NotShown ->
                text ""

            ResultsPopup ->
                let
                    { points, closeness } =
                        getPointsAndCloseness model
                in
                div [ class "popup__background" ]
                    [ div [ class "popup__container" ]
                        [ div [ onClick ToggleSound, class "popup__audio-icon" ]
                            [ text
                                (if model.isMuted then
                                    "🔇"

                                 else
                                    "🔊"
                                )
                            ]
                        , div []
                            [ text <|
                                case closeness of
                                    TriplePerfect ->
                                        "TRIPLE PERFECT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

                                    DoublePerfect ->
                                        "Double perfect!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

                                    Perfect ->
                                        "Perfect!!!!!!1one"

                                    SuperClose ->
                                        "SUPER CLOSE"

                                    Close ->
                                        "Close but no orange"

                                    Far ->
                                        "Not even close..."

                                    SuperFar ->
                                        "Are you even trying?"
                            ]
                        , div []
                            [ text <|
                                case closeness of
                                    TriplePerfect ->
                                        "You get " ++ String.fromInt points ++ " points! You also get an orange token!!!!!!"

                                    DoublePerfect ->
                                        "You get " ++ String.fromInt points ++ " points. If you get one more perfect in a row, you get an orange token!"

                                    Perfect ->
                                        "You get " ++ String.fromInt points ++ " points and you can now sleep at night knowing you're perfect."

                                    SuperClose ->
                                        "You get " ++ String.fromInt points ++ " points."

                                    Close ->
                                        "You get a measly " ++ String.fromInt points ++ " points."

                                    Far ->
                                        "I'll give you 1 point because I'm nice. You don't even have 2 points to rub together... So sad."

                                    SuperFar ->
                                        "You're so far away that I'm giving you 0 points and I'm going to force you to take a 10 minute break."
                            ]
                        , div [] [ text ("You hit " ++ (String.fromInt << round) model.slider) ]
                        , div [ class "popup__button-container" ]
                            [ div [ onClick NextLevel, class "popup__ok-button" ] [ text "OK" ]
                            , if closeness == Far || closeness == SuperFar then
                                div [ onClick Cry, class "popup__cry-button" ] [ text "Cry" ]

                              else
                                text ""
                            ]
                        ]
                    ]

            InfoPopup ->
                div [ class "popup__background" ]
                    [ div [ class "popup__container" ]
                        [ div [ style "font-size" "30px" ] [ text "Game instructions" ]
                        , div [] [ text "You're given a randomly generated target value between 0 and 100." ]
                        , div [] [ text "You also have a slider that goes from 0 to 100." ]
                        , div [] [ text "Try to get the slider's value as close to the target value as possible!" ]
                        , div [] [ text "If you get 3 perfects in a row, you get a free Orange!" ]
                        , div [ onClick ClosePopup, class "popup__ok-button" ] [ text "OK" ]
                        ]
                    ]

            OrangeTokenPopup ->
                div [ class "popup__background" ]
                    [ div [ class "popup__container" ]
                        (if model.orangeTokens > 0 then
                            [ div [ style "font-size" "30px" ] [ text "Trade in Orange Tokens" ]
                            , div [] [ text ("Orange token count: " ++ String.fromInt model.orangeTokens) ]
                            , div [ onClick ClearOrangeTokens, class "popup__ok-button" ] [ text "TRADE IN ORANGE TOKENS" ]
                            , div [ onClick ClosePopup, class "popup__ok-button" ] [ text "CANCEL" ]
                            ]

                         else
                            [ div [ style "font-size" "30px" ] [ text "Trade in Orange Tokens" ]
                            , div [] [ text "You don't have any orange tokens..." ]
                            , div [] [ text "Get 3 perfects in a row and then you'll get an orange token." ]
                            , div [ onClick ClosePopup, class "popup__ok-button" ] [ text "OK" ]
                            ]
                        )
                    ]

            ClearDataPopup ->
                div [ class "popup__background" ]
                    [ div [ class "popup__container" ]
                        [ div [ style "font-size" "30px" ] [ text "Clear All Data?" ]
                        , div [] [ text ("Level: " ++ String.fromInt model.level) ]
                        , div [] [ text ("Score: " ++ String.fromInt model.score) ]
                        , div [] [ text ("Orange token count: " ++ String.fromInt model.orangeTokens) ]
                        , div [ onClick ClearAllData, class "popup__ok-button" ] [ text "CLEAR ALL DATA" ]
                        , div [ onClick ClosePopup, class "popup__ok-button" ] [ text "CANCEL" ]
                        ]
                    ]
        ]



---- PROGRAM ----


main : Program Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
