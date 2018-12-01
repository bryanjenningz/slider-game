port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (style, type_)
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
      }
    , generateRandomTarget
    )


port playSound : String -> Cmd msg


playClosenessSound : Closeness -> Cmd Msg
playClosenessSound closeness =
    let
        soundFile =
            (case closeness of
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


onChange : (Int -> msg) -> Attribute msg
onChange toMsg =
    on "change" (Decode.map toMsg sliderValueDecoder)



---- UPDATE ----


type Msg
    = SetTarget Int
    | SetSlider Int
    | ShowResults
    | NextLevel
    | Cry


type Closeness
    = Perfect
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
                | level = model.level + 1
                , popup = NotShown
                , score = model.score + points
                , orangeTokens =
                    model.orangeTokens
                        + (case closeness of
                            Perfect ->
                                1

                            _ ->
                                0
                          )
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
    div []
        [ input
            [ type_ "range"
            , onChange SetSlider
            , style "width" "80%"
            , style "height" "50px"
            ]
            []
        , div [] [ text ("Slider: " ++ String.fromInt model.slider) ]
        , div [] [ text ("Target: " ++ String.fromInt model.target) ]
        , div [] [ text ("Score: " ++ String.fromInt model.score) ]
        , div [] [ text ("Orange tokens: " ++ String.fromInt model.orangeTokens) ]
        , div [] [ text ("Level: " ++ String.fromInt model.level) ]
        , case model.popup of
            NotShown ->
                button [ onClick ShowResults ] [ text "GO!" ]

            Shown ->
                let
                    { points, closeness } =
                        getPointsAndCloseness model
                in
                div []
                    [ div []
                        [ text <|
                            case closeness of
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
                                Perfect ->
                                    "You get " ++ String.fromInt points ++ " points and you can now sleep at night knowing you're perfect. You get 1 orange token!"

                                SuperClose ->
                                    "You get " ++ String.fromInt points ++ " points."

                                Close ->
                                    "You get a measly " ++ String.fromInt points ++ " points."

                                Far ->
                                    "I'll give you 1 point because I'm nice. You don't even have 2 points to rub together... So sad."

                                SuperFar ->
                                    "You're so far away that I'm giving you 0 points and I'm going to force you to take a 10 minute break."
                        ]
                    , button [ onClick NextLevel ] [ text "OK" ]
                    , if closeness == Far || closeness == SuperFar then
                        button [ onClick Cry ] [ text "Cry" ]

                      else
                        text ""
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
