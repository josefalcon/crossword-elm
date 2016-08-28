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


type alias Dimensions = { width : Int, height : Int }
type alias Position = (Int, Int)
type alias Board = Dict.Dict Position Cell.Model
type alias Answer =
  { locations : List Position
  , clue : String
  , answer : String
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
        [0, 0], [1, 0], [2, 0]
      ],
      "clue": "something vertical",
      "answer": "bar"
    },
    {
      "locations": [
        [0, 0], [0, 1], [0, 2]
      ],
      "clue": "something horizontal",
      "answer": "foo"
    }
  ]
}
"""

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
  object3 Answer
    ("locations" := (list position))
    ("clue" := string)
    ("answer" := string)

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
  | Resize Dimensions
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

    Resize size ->
      ({ model | size = size, board = board size, cursor = (0, 0) } , Cmd.none)

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
  div []
    [ text (toString model)
    , viewBoard model.size model.board model.cursor
    , button [ onClick (Resize (Dimensions 5 5)) ] [ text "5" ]
    , button [ onClick (Resize (Dimensions 9 9)) ] [ text "9" ]
    , button [ onClick (Resize (Dimensions 15 15)) ] [ text "15" ]
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
