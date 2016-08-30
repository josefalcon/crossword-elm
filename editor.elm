import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Keyboard
import Char
import Dict
import List
import Maybe
import String
import Array
import Model exposing (..)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  let
    model =
      { size = { width = 9, height = 9}
      , grid = Array.repeat 9 (Array.repeat 9 Empty)
      , cursor = (0, 0)
      , answers = []
      , direction = Across
      , cellNumbers = Dict.empty
      , message = Nothing
      }
  in
    (model, Cmd.none)


setCursorCell : Cell -> Model -> Grid
setCursorCell cell model =
  setCell model.cursor cell model.grid


-- update


type Msg
  = MoveCursor Int Int
  | SetCursor Position
  | SetCell Cell
  | ChangeDirection
  | NoOp


left : Msg
left = MoveCursor 0 -1


right : Msg
right = MoveCursor 0 1


up : Msg
up = MoveCursor -1 0


down : Msg
down = MoveCursor 1 0


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetCursor (toRow, toCol) ->
      let (row, col) = model.cursor in
        update (MoveCursor (toRow - row) (toCol - col)) model

    MoveCursor rowDelta colDelta ->
      let
        (row, col) = model.cursor
        { width, height } = model.size
        nextRow = row + rowDelta |> min (width - 1) |> max 0
        nextCol = col + colDelta |> min (height - 1) |> max 0
      in
        ({ model | cursor = (nextRow, nextCol) }, Cmd.none)

    SetCell c ->
      let
        nextGrid = setCursorCell c model
        nextModel = { model | grid = nextGrid }
        nextCursor = case (c, model.direction) of
          (Empty, Across) -> left
          (Empty, Down) -> up
          (_, Across) -> right
          _ -> down
      in
        update nextCursor nextModel

    ChangeDirection ->
      ({ model | direction = if (model.direction == Across) then Down else Across }, Cmd.none)

    _ -> (model, Cmd.none)


-- view


view : Model -> Html Msg
view model =
  let
    activeClues = List.filter (\a -> (a.locations |> List.any ((==) model.cursor))) model.answers
  in
    div [ style [ ("padding", "1rem"), ("display", "flex") ] ]
      [ viewGrid model activeClues
      ]


viewGrid : Model -> List Answer -> Html Msg
viewGrid model activeClues =
  let
    activeCells = (List.concatMap .locations (List.filter (\a -> a.direction == model.direction) activeClues))
  in
    div [ style [ ("flex-basis", "100%") ] ]
      [ table [ style [ ("border-collapse", "collapse") ] ]
        [ tbody [] (List.map (viewRow model activeCells) [0..(model.size.height - 1)]) ]
      ]


viewRow : Model -> List Position -> Int -> Html Msg
viewRow model activeCells row =
  let
    activeCell col = List.any ((==) (row, col)) activeCells

    cell col = case (getCell (row, col) model.grid) of
      Just c -> viewCell c (row, col) ((row, col) == model.cursor) (activeCell col)
      Nothing -> td [] []
  in
    tr [] (List.map cell [0..(model.size.width - 1)])


viewCell : Cell -> Position -> Bool -> Bool -> Html Msg
viewCell model position selected active =
  let
    background = if selected then (backgroundColor yellow)
      else if active then (backgroundColor blue)
      else []

    clicker = onClick (SetCursor position)
  in
    case model of
      Empty ->
        td [ clicker, style (cellStyle ++ background) ] [ ]

      Block ->
        td [ clicker, style (cellStyle ++ (backgroundColor "black") ++ background) ] []

      Value c ->
        td [ clicker, style (cellStyle ++ background) ] [ text (String.fromChar c) ]


-- subscriptions


keyCodeToChar : Keyboard.KeyCode -> Msg
keyCodeToChar keyCode =
  case keyCode of
    32 -> ChangeDirection
    37 -> left
    38 -> up
    39 -> right
    40 -> down
    8 -> SetCell Empty
    190 -> SetCell Block
    _ -> if (keyCode >= 65 && keyCode <= 90)
         then keyCode |> Char.fromCode |> Value |> SetCell
         else NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs keyCodeToChar


-- css


yellow : String
yellow = "#ffeb3b"


blue : String
blue = "#b7dbf9"


backgroundColor : String -> List (String, String)
backgroundColor color = [ ("background-color", color) ]


cellStyle : List (String, String)
cellStyle =
  [ ("border", "1px solid black")
  , ("text-align", "center")
  , ("text-transform", "uppercase")
  , ("height", "2rem")
  , ("min-height", "2rem")
  , ("width", "2rem")
  , ("min-width", "2rem")
  , ("font-family", "monospace")
  , ("font-size", "1.5rem")
  , ("position", "relative")
  ]
