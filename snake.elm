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
import Random

import Queue


gridStep : number
gridStep = 20


( gridWidth, gridHeight ) = ( 30, 30 )


( gameWidth, gameHeight ) =
  ( (gridWidth + 1) * gridStep, (gridHeight + 1) * gridStep )


type alias Point =
  { x : Int, y : Int }


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
  , gameOver : Bool
  , controls : Direction -> Direction -> Direction
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
  , gameOver = False
  , controls = absoluteControls
  }


init =
  (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))


-- UPDATE


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


type SnakeMsg
  = Normal
  | Eaten
  | SelfCollision
  | BoundaryCollision


stepSnake : Time -> Snake -> Food -> ( Snake, SnakeMsg )
stepSnake t ({ head, tail, direction } as snake) food =
  let
    move =
      dirToMove direction

    head' =
      { head
        | x = head.x + move.x
        , y = head.y + move.y
      }

    eaten = head.x == food.x && head.y == food.y

    outside
      = head'.x < (-gridWidth // 2)
      || head'.x > (gridWidth // 2)
      || head'.y < (-gridHeight // 2)
      || head'.y > (gridHeight // 2)

    msg =
      if eaten then
        Eaten
      else if List.member head tail then
        SelfCollision
      else if outside then
        BoundaryCollision
      else
        Normal

    tail' =
      if eaten then
        growTail tail head
      else
        moveTail tail head
  in
    ( { snake
        | head = head'
        , tail = tail'
      }
    , msg )


relativeControls : Direction -> Direction -> Direction
relativeControls press previous =
  case ( press, previous ) of
    ( Left, Up ) -> Left
    ( Right, Up ) -> Right

    ( Left, Down ) -> Right
    ( Right, Down ) -> Left

    ( Left, Left ) -> Down
    ( Right, Left ) -> Up

    ( Left, Right ) -> Up
    ( Right, Right ) -> Down

    (_, _) -> previous


absoluteControls : Direction -> Direction -> Direction
absoluteControls press previous =
  case ( press, previous ) of
    -- w
    ( Up, Left ) -> Up
    ( Up, Right ) -> Up
    -- s
    ( Down, Left ) -> Down
    ( Down, Right ) -> Down
    -- a
    ( Left, Up ) -> Left
    ( Left, Down ) -> Left
    -- d
    ( Right, Up ) -> Right
    ( Right, Down ) -> Right

    (_, _) -> previous


type Msg
  = Resize Size
  | ChangeConrols
  | Turn Direction
  | Eat
  | Grow ( Int, Int )
  | Collide
  | Tick Time
  | Pause
  | NoOp


stepGame : Msg -> Game -> ( Game, Cmd Msg )
stepGame msg ({ snake, food, paused, gameOver } as game) =
  case msg of
    NoOp ->
      ( game, Cmd.none )

    Resize size ->
      ( { game | size = size }, Cmd.none )

    ChangeConrols ->
      let
        controls' =
          if game.controls == absoluteControls then
             relativeControls
          else
             absoluteControls
      in
        ( { game | controls = controls' }, Cmd.none )

    Turn direction ->
      let
        direction' =
          game.controls direction snake.direction

        snake' =
          { snake | direction = direction' }
      in
        ( { game | snake = snake' }, Cmd.none )

    Tick delta ->
      if paused then
        ( game, Cmd.none )
      else
        let
          ( snake', msg ) = stepSnake delta snake food

          game' = { game | snake = snake' }
        in
          case msg of
            Eaten ->
              stepGame Eat game'

            SelfCollision ->
              stepGame Collide game

            BoundaryCollision ->
              stepGame Collide game

            Normal ->
              ( game', Cmd.none )

    Eat ->
       ( game
       , Random.generate
                Grow
                <| Random.pair
                    (Random.int (-gridWidth // 2) (gridWidth // 2))
                    (Random.int (-gridHeight // 2) (gridHeight // 2)) )

    Grow (x, y) ->
      ( { game | food = { x = x, y = y } }
      , Cmd.none )

    Collide ->
      stepGame Pause { game | gameOver = True }

    Pause ->
      if gameOver && paused then
        init
      else
        ( { game | paused = not paused }, Cmd.none )

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
    ( toFloat food.x * gridStep, toFloat food.y * gridStep )
    (filled foodColor (rect gridStep gridStep))


displaySnakeHead : Point -> Form
displaySnakeHead { x, y } =
  move
    ( toFloat x * gridStep, toFloat y * gridStep )
    (filled snakeColor (rect (gridStep - 1) (gridStep - 1)))


displaySegment : Point -> Form
displaySegment { x, y } =
  move
    ( toFloat x * gridStep, toFloat y * gridStep )
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
      ( True, 'w' ) -> Turn Up
      ( True, 's' ) -> Turn Down
      ( True, 'a' ) -> Turn Left
      ( True, 'd' ) -> Turn Right
      ( True, 'r' ) -> ChangeConrols
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
