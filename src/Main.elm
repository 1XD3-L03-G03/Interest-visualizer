module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

import Html exposing (input, button, text)
import Html.Attributes exposing (style, type_, value, max, min)
import Html.Events exposing (onInput, onClick)
import String exposing (fromFloat, fromInt)


myShapes model =
    [ titleText
    , questionText model |> move ( 0, 12 )
    , questionInput |> move ( -32, -10 )
    , equationText |> move ( -26, -40 )
    , resultText model |> move ( -10, -5 )
    , bigGraph model |> move ( 5, -24 )
    , controls|> move ( 15, 0)
    , GraphicSVG.text "Money X 1000" |> size 6 |> filled black |> rotate (degrees 90) |> move ( 0, -39 )
    , GraphicSVG.text "Time" |> size 6 |> filled black |> move ( 40, -59 )
    ]


titleText =
    GraphicSVG.text "Interest Calulator" |> centered |> filled black |> move ( 0, 40 )


questionText model =
    group
        [ GraphicSVG.text "Suppose $                is invested at an annual" |> filled black |> scale 0.35 |> move ( -80, 0 )
        , GraphicSVG.text " interest rate of      % for a period" |> filled black |> scale 0.35 |> move ( -80, -5 )
        , GraphicSVG.text "of          years. Compute the future value of " |> filled black |> scale 0.35 |> move ( -80, -10 )
        , GraphicSVG.text "the investment if interest is compounded" |> filled black |> scale 0.35 |> move ( -80, -15 )
        , GraphicSVG.text "continuously:" |> filled black |> scale 0.35 |> move ( -80, -20 )
        , GraphicSVG.text (fromFloat model.valP) |> filled blue |> scale 0.35 |> move ( -62, 0 )
        , GraphicSVG.text (fromFloat model.valR) |> filled blue |> scale 0.35 |> move ( -53, -5 )
        , GraphicSVG.text (fromFloat model.valT) |> filled blue |> scale 0.35 |> move ( -75, -10 )
        ]


questionInput =
    group
        [ html 62 22 (input [ style "border" "0.5px solid grey", style "box-shadow" "inset rgb(0 0 0 / 20%) -2px 2px 4px 0px", type_ "number", Html.Attributes.max "1000000", Html.Attributes.min "1", onInput UpdateP, value "1000", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -20 )
        , html 62 22 (input [ style "border" "0.5px solid grey", style "box-shadow" "inset rgb(0 0 0 / 20%) -2px 2px 4px 0px", type_ "number", Html.Attributes.max "99", Html.Attributes.min "0", onInput UpdateR, value "3", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -25 )
        , html 62 22 (input [ style "border" "0.5px solid grey", style "box-shadow" "inset rgb(0 0 0 / 20%) -2px 2px 4px 0px", type_ "number", Html.Attributes.max "1000", Html.Attributes.min "1", onInput UpdateT, value "10", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -30 )
        , GraphicSVG.text "P" |> filled blue |> scale 0.5 |> move ( -19, -25 )
        , GraphicSVG.text "R" |> filled blue |> scale 0.5 |> move ( -19, -30 )
        , GraphicSVG.text "T" |> filled blue |> scale 0.5 |> move ( -20, -35 )
        , GraphicSVG.text "$ (principal)" |> filled black |> scale 0.25 |> move ( 1, -24 )
        , GraphicSVG.text "% (rate)" |> filled black |> scale 0.25 |> move ( 1, -29 )
        , GraphicSVG.text "years (time)" |> filled black |> scale 0.25 |> move ( 1, -34 )
        ]


equationText =
    group
        [ GraphicSVG.text "B(t) = Pe" |> customFont "Georgia,'Times New Roman',Times,serif" |> italic |> filled black |> scale 0.5 |> move ( -54, 0 )
        , GraphicSVG.text "rt" |> customFont "Georgia,'Times New Roman',Times,serif" |> italic |> filled black |> scale 0.25 |> move ( -30, 3 )
        ]


resultText model =
    GraphicSVG.text ("result after " ++ fromFloat model.valT ++ " years: $" ++ fromInt (round model.result)) |> filled black |> scale 0.5 |> move ( -70, -50 )

controls =
    group
        [
        group
            [
                html 50 30 (button [onClick Grid, style "position" "absolute", style "left" "1px", style "top" "1px",  style "box-shadow" "0px 0.5px 2px -1px rgb(0 0 0 / 30%), 0px 2px 2px 0px rgb(0 0 0 / 20%), 0px 1px 2px 0px rgb(0 0 0 / 15%)"] [Html.text "Grid"]) |> scale 0.5
            ]
            |> move ( 30, -15 )
        , group
            [ html 50 30 (button [onClick Axes , style "position" "absolute", style "left" "1px", style "top" "1px",  style "box-shadow" "0px 0.5px 2px -1px rgb(0 0 0 / 30%), 0px 2px 2px 0px rgb(0 0 0 / 20%), 0px 1px 2px 0px rgb(0 0 0 / 15%)"] [Html.text "Axes"]) |> scale 0.5 
            ]
            |> move ( 70, -15 )
        ]
        |> move ( -25, 30 )


mkCurve model =
    openPolygon
        (List.range 0 200
            |> List.map
                (\idx ->
                    let
                        x = 0.1 * toFloat idx

                        y = (model.valP * (e ^ ((model.valR / 100) * x))) / 1000
                    in
                    ( 4 * x, 1 * y )
                )
         -- valP for princple, valR for rate function inputs
        )
        |> outlined (solid 1) red
        |> move ( 0, 0 )


bigGraph model = group 
  
  [ line (0,0) (100,0)
      |> outlined (solid 0.75) black
  , line (0,0) (0,45)
      |> outlined (solid 0.75) black
  
  , [10,20,30,40]
      |> List.map ( \ idx -> 
                    let
                      x =  2* toFloat idx
                    in [ GraphicSVG.text (String.fromInt idx)
                              |> centered
                              |>bold
                              |> (if (model.axes) then (filled black) else (filled white))
                              |> scale 0.5
                              |> move (x , -6)
                           , line (x, 40) (x,0)
                              |> (if (model.grid) then (outlined (solid 0.5) black) else (outlined (solid 0.5) white))
                              |> scale 1

                       ]
                  )
      |> List.concat 
      |> group
 
  , List.range 0 4 
      |> List.map ( \ idx -> 
                    let
                      y = 10*toFloat  idx
                    in [ GraphicSVG.text (String.fromInt idx)
                              |> bold
                              |> (if (model.axes) then (filled black) else (filled white))
                              |> scale 0.5
                              |> move (2, y)

                           , line (5, y) (96,y)
                              |> (if (model.grid) then (outlined (solid 0.5) black) else (outlined (solid 0.5) white))
                       ]
                  )
      |> List.concat 
      |> group
 
  , mkCurve model
  ] |> move (0, -20)


removeMaybe a =
    case a of
        Just b ->
            b

        Nothing ->
            0


e = 2.71828


type Msg
    = Tick Float GetKeyState
    | UpdateP String
    | UpdateR String
    | UpdateT String
    | Grid
    | Axes


type alias Model =
    { time : Float
    , valP : Float
    , valR : Float
    , valT : Float
    , result : Float
    , grid : Bool
    , axes : Bool
    }


update msg model =
    case msg of
        Tick t _ ->
            { model | time = t
                , valP = if model.valP > 1000000 then  1000000 else (if model.valP < 1 then 1 else model.valP)
                , valR = if model.valR > 99 then  99 else (if model.valR < 0 then 0 else model.valR)
                , valT = if model.valT > 1000 then 1000 else (if model.valT < 1 then 1 else model.valT)
                , result = model.valP * (e ^ ((model.valR / 100) * model.valT))
            }

        UpdateP txt ->
            { model | valP = removeMaybe (String.toFloat txt), result = removeMaybe (String.toFloat txt) * (e ^ ((model.valR / 100) * model.valT)) }

        UpdateR txt ->
            { model | valR = removeMaybe (String.toFloat txt), result = model.valP * (e ^ ((removeMaybe (String.toFloat txt) / 100) * model.valT)) }

        UpdateT txt ->
            { model | valT = removeMaybe (String.toFloat txt), result = model.valP * (e ^ ((model.valR / 100) * removeMaybe (String.toFloat txt)))}

        Grid ->
            { model
                | grid =
                    if model.grid == True then
                        False

                    else
                        True
            }

        Axes ->
            { model
                | axes =
                    if model.axes == True then
                        False

                    else
                        True
            }


init =
    { time = 0
    , valP = 1000.0
    , valR = 3.0
    , valT = 10.0
    , result = 1350.0
    , grid = True
    , axes = True
    }


main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }


view model =
    collage 192 128 (myShapes model)
