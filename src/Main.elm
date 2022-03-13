module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

import Html exposing (input)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import String exposing (fromFloat)
import String exposing (fromInt)


myShapes model =
    [ titleText
    , questionText model |> move ( 0, 30 )
    , questionInput |> move ( -20, 30 )
    , equationText |> move ( -17, 0 )
    , resultText model
    , bigGraph model |> move ( 5, -24 )
    , controls|> move ( 15, 0)
    , text "Money X 1000" |> size 6 |> filled black |> rotate (degrees 90) |> move ( 0, -39 )
    , text "Time" |> size 6 |> filled black |> move ( 40, -59 )
    ]


titleText =
    text "1MM3 - Interest Calulator" |> centered |> filled black |> move ( 0, 40 )


questionText model =
    group
        [ text "Suppose $                is invested at an annual interst rate" |> filled black |> scale 0.25 |> move ( -70, 0 )
        , text "of                % for a period of                years. Compute" |> filled black |> scale 0.25 |> move ( -70, -5 )
        , text "the future value of the investment if interest is" |> filled black |> scale 0.25 |> move ( -70, -10 )
        , text "compunded continuosly:" |> filled black |> scale 0.25 |> move ( -70, -15 )
        , text (fromFloat model.valP) |> filled blue |> scale 0.25 |> move ( -57, 0 )
        , text (fromFloat model.valR) |> filled blue |> scale 0.25 |> move ( -65, -5 )
        , text (fromFloat model.valT) |> filled blue |> scale 0.25 |> move ( -33, -5 )
        ]


questionInput =
    group
        [ html 62 22 (input [ type_ "number", onInput UpdateP, value "1000", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -20 )
        , html 62 22 (input [ type_ "number", onInput UpdateR, value "3", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -25 )
        , html 62 22 (input [ type_ "number", onInput UpdateT, value "10", style "width" "56px", style "height" "16px", style "padding" "0" ] []) |> scale 0.25 |> move ( -15, -30 )
        , text "P" |> filled blue |> scale 0.5 |> move ( -19, -25 )
        , text "R" |> filled blue |> scale 0.5 |> move ( -19, -30 )
        , text "T" |> filled blue |> scale 0.5 |> move ( -20, -35 )
        , text "(principal)" |> filled black |> scale 0.25 |> move ( 1, -24 )
        , text "(rate)" |> filled black |> scale 0.25 |> move ( 1, -29 )
        , text "(time)" |> filled black |> scale 0.25 |> move ( 1, -34 )
        ]


equationText =
    group
        [ text "B(t) = Pe" |> customFont "Georgia,'Times New Roman',Times,serif" |> italic |> filled black |> scale 0.5 |> move ( -54, 0 )
        , text "rt" |> customFont "Georgia,'Times New Roman',Times,serif" |> italic |> filled black |> scale 0.25 |> move ( -30, 3 )
        ]


resultText model =
    text ("result after " ++ fromFloat model.valT ++ " years: " ++ fromInt (round model.result)) |> filled black |> scale 0.5 |> move ( -70, -50 )

controls =
    group
        [
        group
            [ circle 5 |> filled red |> move ( -40, 0 ) |> notifyTap Grid
            , text "Grid" |> filled red |> move ( -72, 0 )
            ]
            |> move ( 80, -20 )
        , group
            [ circle 5 |> filled blue |> move ( -40, -20 ) |> notifyTap Axes
            , text "Axes" |> filled blue |> move ( -72, -20 )
            ]
            |> move ( 130, 0 )
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
                    in [ text (String.fromInt idx)
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
                    in [ text (String.fromInt idx)
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
            { model | time = t }

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
