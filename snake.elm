module Snake exposing (..)

-- MODEL

import Char
import Color exposing (Color, rgb, white, green, black)
import Collage exposing (..)
import Element exposing (Element, toHtml, container, middle)
import Keyboard
import Task
import Time exposing (Time, every, millisecond)
import Window exposing (Size)
import Html.App as App
import Html exposing (Html)

import Queue


gridStep : number
gridStep = 20


( gridWidth, gridHeight ) = ( 30, 30 )


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
  , size : Size
  , paused : Bool
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
  , size = Size 0 0
  , paused = True
  }


init =
  (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))


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


stepSnake : Time -> Snake -> Food -> Snake
stepSnake t ({ head, tail, direction } as snake) food =
  let
    move =
      dirToMove direction

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
    }


type Msg
  = Resize Size
  | Turn Int
  | Tick Time
  | Pause
  | NoOp



stepGame : Msg -> Game -> ( Game, Cmd Msg )
stepGame msg ({ snake, food, paused } as game) =
  case msg of
    NoOp ->
      ( game, Cmd.none )

    Resize size ->
      ( { game | size = size }, Cmd.none )

    Turn relDir ->
      let
        direction' =
          relToAbs relDir snake.direction

        snake' =
          { snake | direction = direction' }
      in
        ( { game | snake = snake' }, Cmd.none )

    Tick delta ->
      if paused then
        ( game, Cmd.none )
      else
        ( { game
            | snake = stepSnake delta snake food }
        , Cmd.none )

    Pause ->
      ( { game
          | paused = not paused }
      , Cmd.none )


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


view : Game -> Html Msg
view game =
  let
    {width, height} = game.size
  in
    display ( width, height ) game
    |> toHtml


keyboardProcessor down keyCode =
  let
    ch =
      Char.fromCode keyCode
      |> Char.toLower
  in
    case (down, ch) of
      ( True, 'a' ) -> Turn -1
      ( True, 'd' ) -> Turn 1
      ( True, ' ' ) -> Pause
      _ -> NoOp


main =
  App.program
    { init = init
    , update = stepGame
    , view = view
    , subscriptions =
      (\_ -> Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs (keyboardProcessor True)
        , Time.every (100 * millisecond) Tick])
    }
