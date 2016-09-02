import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style, tabindex)
import Html.Events exposing (..)
import Keyboard
import Char
import Dict
import List
import Maybe
import String
import Array exposing (Array)
import Model exposing (setCell, getCell, Grid, Cell(Empty, Block, Value), Dimensions, Position, Direction(Across, Down))
import Suggestions
import Json.Decode


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\s -> Sub.none)
    }


type alias Model =
  { size : Dimensions
  , cursor : Position
  , direction : Direction
  , grid : Grid
  , suggestions : Suggestions.Model
  , activeCells : List Position
  }


init : (Model, Cmd Msg)
init =
  let
    dimensions = { width = 9, height = 9}
    model =
      { size = dimensions
      , grid = Array.repeat 9 (Array.repeat 9 Empty)
      , cursor = (0, 0)
      , direction = Across
      , suggestions = (fst Suggestions.init)
      , activeCells = List.map ((,) 0) [0..8]
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
  | SuggestionsMsg Suggestions.Msg
  | UpdateActiveCells
  | NoOp


left : Msg
left = MoveCursor 0 -1


right : Msg
right = MoveCursor 0 1


up : Msg
up = MoveCursor -1 0


down : Msg
down = MoveCursor 1 0


activeCells : Model -> List Position
activeCells model =
  let
    left (row, col) = (row, col - 1)
    right (row, col) = (row, col + 1)
    up (row, col) = (row - 1, col)
    down (row, col) = (row + 1, col)

    takeEmpty next pos accum =
      case (getCell pos model.grid) of
        Just Block -> accum
        Nothing -> accum
        _ -> takeEmpty next (next pos) (pos :: accum)
  in
    case model.direction of
      Across ->
        (takeEmpty left model.cursor []) ++ (takeEmpty right (right model.cursor) []) |> List.sortBy snd

      Down ->
        (takeEmpty up model.cursor []) ++ (takeEmpty down (down model.cursor) []) |> List.sortBy fst


activeCellsPattern : Model -> String
activeCellsPattern model =
  let
    pattern cell = case cell of
      Empty -> "."
      Value c -> c |> String.fromChar |> String.toLower
      _ -> ""
    cell loc = getCell loc model.grid |> Maybe.withDefault Empty
  in
    List.map (cell >> pattern) model.activeCells |> String.join ""


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
        update UpdateActiveCells { model | cursor = (nextRow, nextCol) }

    SetCell c ->
      let
        nextGrid = setCursorCell c model
        nextCursor = case (c, model.direction) of
          (Empty, Across) -> left
          (Empty, Down) -> up
          (_, Across) -> right
          _ -> down
      in
        update nextCursor { model | grid = nextGrid }

    ChangeDirection ->
      let
        nextDirection = if (model.direction == Across) then Down else Across
      in
        update UpdateActiveCells { model | direction = nextDirection }

    SuggestionsMsg msg ->
      let
        (nextSuggesstions, cmd) = Suggestions.update msg model.suggestions
      in
        ({ model | suggestions = nextSuggesstions }, Cmd.map (SuggestionsMsg) cmd)

    UpdateActiveCells ->
      let
        nextModel = { model | activeCells = activeCells model }
        pattern = activeCellsPattern nextModel
      in
        update (SuggestionsMsg (Suggestions.SetPattern pattern)) nextModel

    _ -> (model, Cmd.none)


-- view


onKeyUp : (Int -> Msg) -> Attribute Msg
onKeyUp tagger =
  onWithOptions
    "keydown"
    { stopPropagation = True, preventDefault = True }
    (Json.Decode.map tagger keyCode)


view : Model -> Html Msg
view model =
  div [ style [ ("padding", "1rem"), ("display", "flex") ] ]
    [ viewGrid model
    {--}
    , text (toString (activeCells model))
    --}
    , div [ style [("flex-basis", "100%")] ]
        [ App.map (SuggestionsMsg) (Suggestions.view model.suggestions)
        ]
    ]


viewGrid : Model -> Html Msg
viewGrid model =
  div [ onKeyUp keyCodeToChar, tabindex 0, style [ ("flex-basis", "100%"), ("outline", "none") ] ]
    [ table [ style [ ("border-collapse", "collapse") ] ]
      [ tbody [] (List.map (viewRow model) [0..(model.size.height - 1)]) ]
    ]


viewRow : Model -> Int -> Html Msg
viewRow model row =
  let
    activeCell col = List.any ((==) (row, col)) model.activeCells
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
