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
  | DoDodgyDealPressed

update : Msg -> Model -> Model
update msg model =
  case msg of
    SolveCasePressed ->
      let newMoney = model.money + 1 in
      { model | money = newMoney, dodgyDealEnabled = model.dodgyDealEnabled || newMoney >= 50 }
    DoDodgyDealPressed ->
      let newMoney = model.money + 5 in
      { model | money = newMoney }

-- VIEW

money : Model -> Html Msg
money model =
    div [] [ text ("$" ++ (toString model.money)) ]

solveCaseButton : Model -> Html Msg
solveCaseButton model =
    button [ onClick SolveCasePressed , class "btn" ] [ text "Solve case" ]

dodgyDealButton : Model -> Maybe (Html Msg)
dodgyDealButton model =
    case model.dodgyDealEnabled of
        True -> Just <| button [ onClick DoDodgyDealPressed , class "btn" ] [ text "Do dodgy deal" ]
        False -> Nothing

view : Model -> Html Msg
view model =
  div [class "bk"]
    (List.filterMap 
        identity
        [ Just <| money model
        , Just <| solveCaseButton model 
        , dodgyDealButton model
        ]
    )
