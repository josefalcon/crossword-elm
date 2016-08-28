import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Keyboard
import Char
import Dict
import List
import Maybe
import Cell
import Json.Decode exposing (..)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Direction
  = Across
  | Down

type alias Dimensions = { width : Int, height : Int }
type alias Position = (Int, Int)
type alias Board = Dict.Dict Position Cell.Model
type alias Answer =
  { locations : List Position
  , clue : String
  , answer : String
  , direction : Direction
  }

-- decoders
example = """
{
  "size": {
    "width": 5,
    "height": 5
  },
  "answers": [
    {
      "locations": [
        [0, 0], [0, 1], [0, 2]
      ],
      "clue": "something horizontal",
      "answer": "foo",
      "direction": "across"
    },
    {
      "locations": [
        [1, 0], [1, 1], [1, 2], [1, 3]
      ],
      "clue": "something horizontal",
      "answer": "foo",
      "direction": "across"
    },
    {
      "locations": [
        [2, 0], [2, 1], [2, 2], [2, 3], [2, 4]
      ],
      "clue": "something horizontal",
      "answer": "foo",
      "direction": "across"
    },
    {
      "locations": [
        [3, 1], [3, 2], [3, 3], [3, 4]
      ],
      "clue": "something horizontal",
      "answer": "foo",
      "direction": "across"
    },
    {
      "locations": [
        [4, 2], [4, 3], [4, 4]
      ],
      "clue": "something horizontal",
      "answer": "foo",
      "direction": "across"
    },
    {
      "locations": [
        [0, 0], [1, 0], [2, 0]
      ],
      "clue": "something vertical",
      "answer": "bar",
      "direction": "down"
    },
    {
      "locations": [
        [0, 1], [1, 1], [2, 1], [3, 1]
      ],
      "clue": "something vertical",
      "answer": "bar",
      "direction": "down"
    },
    {
      "locations": [
        [0, 2], [1, 2], [2, 2], [3, 2], [4, 2]
      ],
      "clue": "something vertical",
      "answer": "bar",
      "direction": "down"
    },
    {
      "locations": [
        [1, 3], [2, 3], [3, 3], [4, 3]
      ],
      "clue": "something vertical",
      "answer": "bar",
      "direction": "down"
    },
    {
      "locations": [
        [2, 4], [3, 4], [4, 4]
      ],
      "clue": "something vertical",
      "answer": "bar",
      "direction": "down"
    }
  ]
}
"""
directionSpecific : String -> Decoder Direction
directionSpecific s =
  case s of
    "across" -> map (always Across) string
    _ -> map (always Down) string


direction : Decoder Direction
direction =
  string `andThen` directionSpecific


dimensions : Decoder Dimensions
dimensions =
  object2 Dimensions
    ("width" := int)
    ("height" := int)

position : Decoder Position
position =
  tuple2 (,) int int

answer : Decoder Answer
answer =
  object4 Answer
    ("locations" := (list position))
    ("clue" := string)
    ("answer" := string)
    ("direction" := direction)

puzzle : Decoder Model
puzzle =
  object2 (\a -> (\s -> { size = s, board = (board s), cursor = (0, 0), answers = a }))
    ("answers" := (list answer))
    ("size" := dimensions)

--


type alias Model =
  { size : Dimensions
  , board : Board
  , answers : List Answer
  , cursor : Position
  }


init : (Model, Cmd Msg)
init =
  let
    result = decodeString puzzle example
  in
    case result of
      Err err ->
        Debug.crash err

      Ok model ->
        let
          chip loc b = Dict.insert loc Cell.Empty b
          chipAll ans b = List.foldl chip b ans.locations
        in
          ({ model | board = List.foldl chipAll model.board model.answers }, Cmd.none)


cartesian : List a -> List b -> List (a, b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs


board : Dimensions -> Board
board size =
  Dict.fromList (List.map (\x -> (x, Cell.Block)) (cartesian [0..(size.height - 1)] [0..(size.width - 1)]))


activeAnswers : Model -> List Answer
activeAnswers model =
  let
    f = \a -> (List.any (\x -> model.cursor == x) a.locations)
  in
    List.filter f model.answers


type Msg
  = MoveCursor Int Int
  | SetCell Char
  | DeleteCell
  | CellMsg Position Cell.Msg
  | Nothing


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MoveCursor rowDelta colDelta ->
      let
        (row, col) = model.cursor
        rowBound = \v -> (max 0 (min (model.size.width - 1) v))
        colBound = \v -> (max 0 (min (model.size.height - 1) v))
      in
        ({ model | cursor = (rowBound (row + rowDelta), colBound (col + colDelta)) }, Cmd.none)

    SetCell c ->
      case (Dict.get model.cursor model.board) of
        Just Cell.Block -> (model, Cmd.none)
        _ -> ({ model | board = (Dict.insert model.cursor (Cell.Value c) model.board) } , Cmd.none)

    DeleteCell ->
      case (Dict.get model.cursor model.board) of
        Just Cell.Block -> (model, Cmd.none)
        _ -> ({ model | board = (Dict.insert model.cursor Cell.Empty model.board) } , Cmd.none)

    CellMsg pos Cell.Click ->
      ({ model | cursor = pos }, Cmd.none)

    _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  div [ style [ ("padding", "1rem") ] ]
    [ viewBoard model.size model.board model.cursor
    , h2 [] [ text "Across" ]
    , viewClues model Across
    , h2 [] [ text "Down" ]
    , viewClues model Down
    , text (model |> activeAnswers |> toString)
    ]

viewBoard : Dimensions -> Board -> Position -> Html Msg
viewBoard size board cursor =
  table [ style [("border-collapse", "collapse")] ]
    [ tbody [] (List.map (viewRow size.width board cursor) [0..(size.height - 1)]) ]


viewRow : Int -> Board -> Position -> Int -> Html Msg
viewRow size board cursor row =
  let
    cell col = case (Dict.get (row, col) board) of
      Maybe.Just c -> App.map (CellMsg (row, col)) (Cell.view c ((row, col) == cursor))
      Maybe.Nothing -> td [] []
  in
    tr [] (List.map cell [0..(size - 1)])


viewClues : Model -> Direction -> Html Msg
viewClues model direction =
  let
    activeClues = activeAnswers model
    isActiveClue c = List.any ((==) c) activeClues
    clues = model.answers |> List.filter (\s -> s.direction == direction)
  in
    div []
      [ ul [] (List.map (\c -> li [ style (if (isActiveClue c) then [("background-color", "red")] else []) ] [ text c.clue ]) clues)
      ]


keyCodeToChar : Keyboard.KeyCode -> Msg
keyCodeToChar keyCode =
  case keyCode of
    37 -> MoveCursor 0 -1
    38 -> MoveCursor -1 0
    39 -> MoveCursor 0 1
    40 -> MoveCursor 1 0
    8 -> DeleteCell
    _ -> keyCode |> Char.fromCode |> SetCell


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs keyCodeToChar
