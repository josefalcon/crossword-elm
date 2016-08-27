import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List
import Keyboard
import Char

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { size : Int
  , board : List (List String)
  , cursor : (Int, Int)
  }

init : (Model, Cmd Msg)
init =
  ({ size = 5, board = (board 5), cursor = (0, 0) }, Cmd.none)

board : Int -> List (List String)
board size =
  List.repeat size (List.repeat size "a")


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


cursor : List (String, String)
cursor =
  [ ("background-color", "yellow")
  ]


view : Model -> Html Msg
view model =
  div []
    [ text (toString model) 
    , viewBoard model.cursor model.board
    , button [ onClick (Resize 5) ] [ text "5" ]
    , button [ onClick (Resize 9) ] [ text "9" ]
    , button [ onClick (Resize 15) ] [ text "15" ]
    ]

viewBoard : (Int, Int) -> List (List String) -> Html Msg
viewBoard cursor board =
  table [ style [("border-collapse", "collapse")] ]
    [ tbody [] (List.indexedMap (viewRow cursor) board) ]


viewRow : (Int, Int) -> Int -> List String -> Html Msg
viewRow (cursorRow, cursorCol) index row =
  let
    isCursorRow = index == cursorRow
    cursorStyle col = if isCursorRow && col == cursorCol then cursor else []
    cell col val = td [ style (letterBox ++ (cursorStyle col)) ] [ text val ]
  in
    tr [] (List.indexedMap cell row)


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
