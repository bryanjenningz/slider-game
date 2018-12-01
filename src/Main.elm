module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Random



---- MODEL ----


type alias Model =
    { slider : Int
    , target : Int
    , score : Int
    , level : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { slider = 50, target = 0, score = 0, level = 1 }, generateRandomTarget )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTarget target ->
            ( { model | target = target }, Cmd.none )

        SetSlider slider ->
            ( { model | slider = slider }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "range", onChange SetSlider, style "width" "80%", style "height" "50px" ] []
        , div [] [ text ("Slider: " ++ String.fromInt model.slider) ]
        , div [] [ text ("Target: " ++ String.fromInt model.target) ]
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
