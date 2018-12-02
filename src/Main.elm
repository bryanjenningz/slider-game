port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (attribute, class, style, type_, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Random



---- MODEL ----


type Popup
    = NotShown
    | Shown


type alias Model =
    { slider : Int
    , target : Int
    , score : Int
    , orangeTokens : Int
    , closenessHistory : List Closeness
    , level : Int
    , popup : Popup
    , isSliderMouseDown : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { slider = 50
      , target = 0
      , score = 0
      , orangeTokens = 0
      , closenessHistory = []
      , level = 1
      , popup = NotShown
      , isSliderMouseDown = False
      }
    , generateRandomTarget
    )


port playSound : String -> Cmd msg


playClosenessSound : Closeness -> Cmd Msg
playClosenessSound closeness =
    let
        soundFile =
            (case closeness of
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
            )
                ++ ".m4a"
    in
    playSound soundFile


generateRandomTarget : Cmd Msg
generateRandomTarget =
    Random.generate SetTarget (Random.int 0 100)


sliderValueDecoder : Decoder Int
sliderValueDecoder =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.map (String.toInt >> Maybe.withDefault 0)


onSliderChange : (Int -> msg) -> Attribute msg
onSliderChange toMsg =
    on "change" (Decode.map toMsg sliderValueDecoder)



---- UPDATE ----


type Msg
    = SetTarget Int
    | SetSlider Int
    | ShowResults
    | NextLevel
    | Cry


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
        SetTarget target ->
            ( { model | target = target }, Cmd.none )

        SetSlider slider ->
            ( { model | slider = slider }, Cmd.none )

        ShowResults ->
            let
                { closeness } =
                    getPointsAndCloseness model
            in
            ( { model | popup = Shown }, playClosenessSound closeness )

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
            ( model, playSound "cry.m4a" )



---- VIEW ----


getPointsAndCloseness model =
    let
        sliderDifference =
            abs (model.slider - model.target)

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
            [ div [] [ text ("Level: " ++ String.fromInt model.level) ]
            , div [] [ text ("Score: " ++ String.fromInt model.score) ]
            , div [] [ text ("Oranges: " ++ String.fromInt model.orangeTokens) ]
            ]
        , div [ class "target-info" ] [ text ("Target: " ++ String.fromInt model.target) ]
        , input
            [ type_ "range"
            , value (String.fromInt model.slider)
            , onSliderChange SetSlider
            , attribute "aria-label" "game slider"
            ]
            []
        , div [ onClick ShowResults, class "show-results__button" ] [ text "GO!" ]
        , case model.popup of
            NotShown ->
                text ""

            Shown ->
                let
                    { points, closeness } =
                        getPointsAndCloseness model
                in
                div [ class "popup__background" ]
                    [ div [ class "popup__container" ]
                        [ div []
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
                        , div [] [ text ("You hit " ++ String.fromInt model.slider) ]
                        , div [ class "popup__button-container" ]
                            [ div [ onClick NextLevel, class "popup__ok-button" ] [ text "OK" ]
                            , if closeness == Far || closeness == SuperFar then
                                div [ onClick Cry, class "popup__cry-button" ] [ text "Cry" ]

                              else
                                text ""
                            ]
                        ]
                    ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
