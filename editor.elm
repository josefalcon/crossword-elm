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
import Array exposing (Array)
import Model exposing (setCell, getCell, Grid, Cell(Empty, Block, Value), Dimensions, Position, Direction(Across, Down))
import Suggestions


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Slat =
  { direction : Direction
  , locations : List Position
  }


type alias Model =
  { size : Dimensions
  , cursor : Position
  , direction : Direction
  , grid : Grid
  , suggestions : Suggestions.Model
  , slats : Array Slat
  , activeSlat : Maybe Slat
  }


initSlats : Dimensions -> Array Slat
initSlats dimensions =
  let
    across row = List.map (\col -> (row, col)) [0..dimensions.width]
    down col = List.map (\row -> (row, col)) [0..dimensions.height]
  in
    Array.fromList
      ( (List.map across [0..dimensions.height] |> List.map (Slat Across))
        ++
        (List.map down [0..dimensions.width] |> List.map (Slat Down))
      )


init : (Model, Cmd Msg)
init =
  let
    dimensions = { width = 9, height = 9}
    slats = initSlats dimensions
    model =
      { size = dimensions
      , grid = Array.repeat 9 (Array.repeat 9 Empty)
      , cursor = (0, 0)
      , direction = Across
      , suggestions = (fst Suggestions.init)
      , slats = slats
      , activeSlat = Array.get 0 slats
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
  | NoOp


left : Msg
left = MoveCursor 0 -1


right : Msg
right = MoveCursor 0 1


up : Msg
up = MoveCursor -1 0


down : Msg
down = MoveCursor 1 0


activeSlat : Position -> Direction -> Array Slat -> Maybe Slat
activeSlat cursor direction slats =
  let
    containsCursor slat =
      (slat.direction == direction) && (slat.locations |> List.any ((==) cursor))
  in
    Array.get 0 (Array.filter containsCursor slats)


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
        nextCursor = (nextRow, nextCol)
        nextActiveSlat = activeSlat nextCursor model.direction model.slats
      in
        ({ model | cursor = nextCursor, activeSlat = nextActiveSlat }, Cmd.none)

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
      let
        nextDirection = if (model.direction == Across) then Down else Across
        nextActiveSlat = activeSlat model.cursor nextDirection model.slats
      in
        ({ model | direction = nextDirection, activeSlat = nextActiveSlat }, Cmd.none)

    SuggestionsMsg msg ->
      let
        (nextSuggesstions, cmd) = Suggestions.update msg model.suggestions
      in
        ({ model | suggestions = nextSuggesstions }, Cmd.map (SuggestionsMsg) cmd)

    _ -> (model, Cmd.none)


-- view


view : Model -> Html Msg
view model =
  div [ style [ ("padding", "1rem"), ("display", "flex") ] ]
    [ viewGrid model
    {--
    , text (toString model)
    --}
    , div [ style [("flex-basis", "100%")] ]
        [ App.map (SuggestionsMsg) (Suggestions.view model.suggestions)
        ]
    ]


viewGrid : Model -> Html Msg
viewGrid model =
  div [ style [ ("flex-basis", "100%") ] ]
    [ table [ style [ ("border-collapse", "collapse") ] ]
      [ tbody [] (List.map (viewRow model) [0..(model.size.height - 1)]) ]
    ]


viewRow : Model -> Int -> Html Msg
viewRow model row =
  let
    cell col = case (getCell (row, col) model.grid) of
      Just c -> viewCell c (row, col) ((row, col) == model.cursor)
      Nothing -> td [] []
  in
    tr [] (List.map cell [0..(model.size.width - 1)])


viewCell : Cell -> Position -> Bool -> Html Msg
viewCell model position selected =
  let
    background = if selected then (backgroundColor yellow) else []

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
