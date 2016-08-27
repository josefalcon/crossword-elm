import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Keyboard
import Char
import Dict
import List
import Maybe

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Position = (Int, Int)
type alias Board = Dict.Dict (Int, Int) String

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
  Dict.fromList (List.map (\x -> (x, "a")) (cartesian [0..(size - 1)] [0..(size - 1)]))


type Msg
  = MoveCursor Int Int
  | Resize Int


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


letterBox : List (String, String)
letterBox =
    [ ("border", "1px solid black")
    , ("text-align", "center")
    , ("text-transform", "uppercase")
    , ("height", "2rem")
    , ("width", "2rem")
    , ("font-family", "monospace")
    , ("font-size", "1.5rem")
    ]


yellow : List (String, String)
yellow =
  [ ("background-color", "yellow")
  ]


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
    cursorStyle col = if (row, col) == cursor then yellow else []
    cell col = td [ style (letterBox ++ (cursorStyle col)) ] [ text (Maybe.withDefault "a" (Dict.get (row, col) board)) ]
  in
    tr [] (List.map cell [0..(size - 1)])


keyCodeToChar : Keyboard.KeyCode -> Msg
keyCodeToChar keyCode =
  case keyCode of
    37 -> MoveCursor 0 -1
    38 -> MoveCursor -1 0
    39 -> MoveCursor 0 1
    40 -> MoveCursor 1 0
    _ -> MoveCursor 0 0

subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs keyCodeToChar
