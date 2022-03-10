module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

import Html exposing (input)
import Html.Attributes exposing (type_,value,style)
import Html.Events exposing (onInput)
import String exposing (fromFloat)

myShapes model =
  [ 
    titleText
    , questionText model |> move (0,30)
    , questionInput |> move (0,30)
  ]

titleText = text "1MM3 - Interest Calulator" |> centered |> filled black |> move (0,40) 
questionText model = group [
  text "Suppose $                is invested at an annual interst rate" |> filled black  |> scale 0.25 |> move (-70,0) 
  , text "of                % for a period of                years. Compute"  |> filled black |> scale 0.25 |> move (-70,-5) 
  , text "the future value of the investment if interest is" |> filled black |> scale 0.25 |> move (-70,-10) 
  , text "compunded continuosly:" |> filled black |> scale 0.25 |> move (-70,-15)
  , text (fromFloat model.valP) |> filled blue  |> scale 0.25 |> move (-57,0) 
  , text (fromFloat model.valI) |> filled blue  |> scale 0.25 |> move (-65,-5) 
  , text (fromFloat model.valT) |> filled blue  |> scale 0.25 |> move (-33,-5) 
  ]

questionInput = group [
    html 62 22 (input [type_ "number", onInput UpdateP, value "1000", style "width" "56px", style "height" "16px", style "padding" "0"] []) |> scale 0.25 |> move (-15,-20)
    , html 62 22 (input [type_ "number", onInput UpdateI, value "3", style "width" "56px", style "height" "16px", style "padding" "0"] []) |> scale 0.25 |> move (-15,-25)
    , html 62 22 (input [type_ "number", onInput UpdateT, value "5", style "width" "56px", style "height" "16px", style "padding" "0"] []) |> scale 0.25 |> move (-15,-30)
    , text "P" |> filled blue  |> scale 0.5 |> move (-19,-25)
    , text "I" |> filled blue  |> scale 0.5 |> move (-19,-30)
    , text "T" |> filled blue  |> scale 0.5 |> move (-20,-35)
  ]

removeMaybe a = 
  case a of
    Just b -> b
    Nothing -> 0

type Msg = Tick Float GetKeyState
  | UpdateP String
  | UpdateI String
  | UpdateT String

type alias Model = { 
  time : Float 
  , valP : Float
  , valI : Float
  , valT : Float
  }

update msg model = case msg of
                      Tick t _ -> { model | time = t }
                      
                      UpdateP txt -> {model | valP = removeMaybe (String.toFloat txt)}
                      UpdateI txt -> {model | valI = removeMaybe (String.toFloat txt)}
                      UpdateT txt -> {model | valT = removeMaybe (String.toFloat txt)}

init = { 
  time = 0 
  , valP = 1000.0
  , valI = 3.0
  , valT = 10.0
  }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



