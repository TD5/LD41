module NeoNoirClicker exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- Neo-Noir Clicker; "Two Incompatible Genres"

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model = 
    {
        money : Int,
        dodgyDealEnabled : Bool
    }


model : Model
model =
    {
        money = 0,
        dodgyDealEnabled = False
    }


-- UPDATE


type Msg
  = SolveCasePressed

update : Msg -> Model -> Model
update msg model =
  case msg of
    SolveCasePressed ->
      let newMoney = model.money + 1 in
      { model | money = newMoney, dodgyDealEnabled = model.dodgyDealEnabled || newMoney >= 50 }

-- VIEW


view : Model -> Html Msg
view model =
  div [class "bk"]
    [ div [] [ text ("$" ++ (toString model.money)) ]
    , button [ onClick SolveCasePressed , class "btn" ] [ text "Solve case" ]
    ]
