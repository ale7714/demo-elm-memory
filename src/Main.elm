module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder)
import String


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { player : String
    , counter : Int
    , cards : List String
    }


model : Model
model =
    Model "" 0 []



-- UPDATE


type Msg
    = UpdatePlayer String
    | ShuffleCards


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatePlayer newPlayer ->
            { model | player = newPlayer }

        ShuffleCards ->
            { model | cards = [ "A", "C", "B", "A", "B", "C" ] }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Player name", onInput UpdatePlayer ] []
        , div [] [ text model.player ]
        , button [ onClick ShuffleCards ] [ text "Shuffle cards" ]
        , div [] [ text (String.join ", " model.cards) ]
        ]
