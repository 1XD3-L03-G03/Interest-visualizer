module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model =
  [
    circle 5 |> filled red
      |> move (20 * sin model.time, 0)
  ]

type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



