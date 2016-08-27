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


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Position = (Int, Int)
type alias Board = Dict.Dict Position Cell.Model


type alias Model =
  { size : Int
  , board : Board
  , cursor : Position
  }


init : (Model, Cmd Msg)
init =
  ({ size = 5, board = (board 5), cursor = (0, 0) }, Cmd.none)

cartesian : List a -> List b -> List (a, b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

board : Int -> Board
board size =
  Dict.fromList (List.map (\x -> (x, Cell.init)) (cartesian [0..(size - 1)] [0..(size - 1)]))


type Msg
  = MoveCursor Int Int
  | Resize Int
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
        bound = \v -> (max 0 (min (model.size - 1) v))
      in
        ({ model | cursor = (bound (row + rowDelta), bound (col + colDelta)) }, Cmd.none)

    Resize size ->
      ({ model | size = size, board = board size, cursor = (0, 0) } , Cmd.none)

    SetCell c ->
      ({ model | board = (Dict.insert model.cursor (Cell.Value c) model.board) } , Cmd.none)

    DeleteCell ->
      ({ model | board = (Dict.insert model.cursor Cell.Empty model.board) } , Cmd.none)

    CellMsg pos Cell.Click ->
      ({ model | cursor = pos }, Cmd.none)

    _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ text (toString model)
    , viewBoard model.size model.board model.cursor
    , button [ onClick (Resize 5) ] [ text "5" ]
    , button [ onClick (Resize 9) ] [ text "9" ]
    , button [ onClick (Resize 15) ] [ text "15" ]
    ]

viewBoard : Int -> Board -> Position -> Html Msg
viewBoard size board cursor =
  table [ style [("border-collapse", "collapse")] ]
    [ tbody [] (List.map (viewRow size board cursor) [0..(size - 1)]) ]


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
