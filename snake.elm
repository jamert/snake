module Snake (..) where

-- MODEL

import Color exposing (Color, rgb, white, green, black)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, container, middle)
import Keyboard
import Time exposing (Time, fps, inSeconds)
import Window
import Datastructures.Queue as Queue


gridStep : number
gridStep =
  20


( gridWidth, gridHeight ) =
  ( 30, 30 )


( gameWidth, gameHeight ) =
  ( (gridWidth + 1) * gridStep, (gridHeight + 1) * gridStep )


type alias Point =
  { x : Float, y : Float }


type Direction
  = Up
  | Down
  | Left
  | Right


type alias Snake =
  { head : Point
  , tail : List Point
  , direction : Direction
  }


type alias Food =
  Point


type alias Game =
  { snake : Snake
  , food : Food
  }


makeSnake : Point -> Snake
makeSnake hd =
  { head = hd
  , tail =
      [ { x = hd.x - 1, y = hd.y }
      , { x = hd.x - 2, y = hd.y }
      , { x = hd.x - 2, y = hd.y - 1 }
      , { x = hd.x - 2, y = hd.y - 2 }
      , { x = hd.x - 3, y = hd.y - 2 }
      ]
  , direction = Right
  }


defaultGame : Game
defaultGame =
  { snake = makeSnake { x = 1, y = 0 }
  , food = { x = 5, y = 3 }
  }



-- INPUTS


type alias Input =
  { space : Bool
  , turn : Int
  , delta : Time
  }


delta : Signal Time
delta =
  Signal.map inSeconds (fps 10)


input : Signal Input
input =
  Signal.sampleOn delta
    <| Signal.map3
        Input
        Keyboard.space
        (Signal.map .x Keyboard.wasd)
        delta



-- UPDATE


relToAbs : Int -> Direction -> Direction
relToAbs turn dir =
  case ( turn, dir ) of
    ( -1, Up ) ->
      Left

    ( 1, Up ) ->
      Right

    ( -1, Down ) ->
      Right

    ( 1, Down ) ->
      Left

    ( -1, Left ) ->
      Down

    ( 1, Left ) ->
      Up

    ( -1, Right ) ->
      Up

    ( 1, Right ) ->
      Down

    ( _, dir ) ->
      dir


dirToMove : Direction -> Point
dirToMove dir =
  case dir of
    Up ->
      { x = 0, y = 1 }

    Down ->
      { x = 0, y = -1 }

    Left ->
      { x = -1, y = 0 }

    Right ->
      { x = 1, y = 0 }


moveTail : List Point -> Point -> List Point
moveTail tail head =
  let
    ( cut, tailQueue ) =
      Queue.enqueue head (Queue.fromList tail)
        |> Queue.dequeue
  in
    Queue.toList tailQueue


growTail : List Point -> Point -> List Point
growTail tail head =
  [ head ] `List.append` tail


stepSnake : Time -> Int -> Snake -> Food -> Snake
stepSnake t turn ({ head, tail, direction } as snake) food =
  let
    direction' =
      relToAbs turn direction

    move =
      dirToMove direction'

    head' =
      { head
        | x = clamp -(gridWidth / 2) (gridWidth / 2) (head.x + move.x)
        , y = clamp -(gridHeight / 2) (gridHeight / 2) (head.y + move.y)
      }

    tail' =
      if head.x == food.x && head.y == food.y then
        growTail tail head
      else
        moveTail tail head
  in
    { snake
      | head = head'
      , tail = tail'
      , direction = direction'
    }


stepGame : Input -> Game -> Game
stepGame input game =
  let
    { turn, delta } =
      input

    { snake, food } =
      game
  in
    { game
      | snake = stepSnake delta turn snake food
    }


gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input



-- VIEW


fieldColor : Color
fieldColor =
  rgb 170 10 10


foodColor : Color
foodColor =
  black


snakeColor : Color
snakeColor =
  white


displayFood : Food -> Form
displayFood food =
  move
    ( food.x * gridStep, food.y * gridStep )
    (filled foodColor (rect gridStep gridStep))


displaySnakeHead : Point -> Form
displaySnakeHead { x, y } =
  move
    ( x * gridStep, y * gridStep )
    (filled snakeColor (rect (gridStep - 1) (gridStep - 1)))


displaySegment : Point -> Form
displaySegment { x, y } =
  move
    ( x * gridStep, y * gridStep )
    (filled snakeColor (rect (gridStep - 1) (gridStep - 1)))


displaySnakeTail : List Point -> Form
displaySnakeTail tail =
  List.map displaySegment tail |> group


displaySnake : Snake -> Form
displaySnake { head, tail } =
  group
    [ displaySnakeHead head
    , displaySnakeTail tail
    ]


display : ( Int, Int ) -> Game -> Element
display ( w, h ) { snake, food } =
  container w h middle
    <| collage
        gameHeight
        gameWidth
        [ filled fieldColor (rect gameWidth gameHeight)
        , displayFood food
        , displaySnake snake
        ]


main : Signal Element
main =
  Signal.map2 display Window.dimensions gameState
