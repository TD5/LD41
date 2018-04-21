module NeoNoirClicker exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Time exposing (Time)

-- Neo-Noir Clicker; "Two Incompatible Genres"

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type Rank 
    = Officer
    | Corporal


type alias Model = 
    {
        startTime : Maybe Time,
        time : Time,
        money : Int,
        dodgyDealEnabled : Bool,
        suspiciousness : Int,
        rank : Rank,
        corruptOfficers : Int
    }


init : (Model, Cmd Msg)
init =
    let model =
        {
            startTime = Nothing,
            time = 0,
            money = 0,
            dodgyDealEnabled = False,
            suspiciousness = 0,
            rank = Officer,
            corruptOfficers = 0
        }
    in
        (model, Cmd.none)

corruptOfficerCost : Int
corruptOfficerCost = 250


-- UPDATE


type Msg
  = Tick Time
  | SolveCase
  | DoDodgyDeal
  | TakePromotion
  | HireCorruptOfficer

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond * 500) Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let newModel =
      case msg of
        SolveCase ->
          let newMoney = model.money + 1 in
          { model | 
                money = newMoney, 
                dodgyDealEnabled = model.dodgyDealEnabled || newMoney >= 50,
                suspiciousness = max 0 (model.suspiciousness - 1)
          }
        DoDodgyDeal ->
          { model |
                money = model.money + 5,
                suspiciousness = model.suspiciousness + 3
          }
        TakePromotion ->
          { model | rank = Corporal }
        HireCorruptOfficer ->
          { model | 
                money = model.money - corruptOfficerCost, 
                corruptOfficers = model.corruptOfficers + 1
          }       
        Tick newTime ->
          { model | 
                time = newTime,
                startTime = Just <| Maybe.withDefault newTime model.startTime,
                money = model.money + model.corruptOfficers
          }
    in
        (newModel, Cmd.none)

-- VIEW

type Suspiciousness
    = ExcellentStanding
    | GoodStanding
    | Shady
    | Dodgy
    | Villain

viewSuspiciousness : Suspiciousness -> String
viewSuspiciousness suspiciousness =
    case suspiciousness of
        ExcellentStanding -> "You're in excellent standing"
        GoodStanding      -> "No one suspects anything"
        Shady             -> "You're looking a bit shady"
        Dodgy             -> "The Feds know you're up to something"
        Villain           -> "You're well known to be a crook. It's only a matter of time..."
        

status : Model -> Html Msg
status model =
    let suspicion =
        let suspiciousness =
            if model.suspiciousness > 1000 then
                Villain         
            else if model.suspiciousness > 300 then
                Dodgy
            else if model.suspiciousness > 100 then
                Shady
            else if model.suspiciousness > 30 then
                GoodStanding
            else
                ExcellentStanding
        in
            if model.dodgyDealEnabled then
                " | " ++ viewSuspiciousness suspiciousness 
            else
                ""
        corruptOfficers =
            case model.corruptOfficers of
                0 -> ""
                1 -> " + 1 corrupt underling"
                n -> " + " ++ (toString n) ++ " corrupt underlings"
    in
        div [] [ text ("$" ++ (toString model.money) ++ " [" ++ (toString model.rank) ++ corruptOfficers ++ "] " ++ suspicion) ]

solveCaseButton : Model -> Html Msg
solveCaseButton model =
    button [ onClick SolveCase , class "btn" ] [ text "Solve case" ]

dodgyDealButton : Model -> Maybe (Html Msg)
dodgyDealButton model =
    case model.dodgyDealEnabled of
        True -> Just <| button [ onClick DoDodgyDeal , class "btn" ] [ text "Do dodgy deal" ]
        False -> Nothing

takePromotionButton : Model -> Maybe (Html Msg)
takePromotionButton model =
    case model.startTime of

        Just start -> 
            if (model.time > start + (30 * Time.second) && model.rank == Officer) then
                Just <| button [ onClick TakePromotion, class "btn" ] [ text "Take promotion" ]
            else
                Nothing

        Nothing -> Nothing
        
hireCorruptOfficerButton : Model -> Maybe (Html Msg)
hireCorruptOfficerButton model =
    case model.rank of
        Officer -> 
            Nothing
        Corporal -> 
            Just <| 
                button 
                    [ disabled (model.money < corruptOfficerCost ), onClick HireCorruptOfficer, class "btn" ] 
                    [ text ("Hire corrupt officer: $" ++ (toString corruptOfficerCost)) ]

view : Model -> Html Msg
view model =
    div []
        [ status model
        , div [ class "allButtons" ]
            [
              div [ class "row directActions" ]
                (List.filterMap 
                    identity
                    [ Just <| solveCaseButton model 
                    , dodgyDealButton model
                    ]
                ),
              div [ class "row buyActions" ]
                (List.filterMap 
                    identity
                    [ takePromotionButton model
                    , hireCorruptOfficerButton model
                    ]
                )
            ]
        ]
