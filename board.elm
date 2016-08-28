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
{"size":{"width":9,"height":9},"answers":[{"locations":[[0,0],[0,1],[0,2],[0,3]],"clue":"Vegetarian option","answer":"tofu","direction":"across"},{"locations":[[0,5],[0,6],[0,7],[0,8]],"clue":"Item on a desktop","answer":"icon","direction":"across"},{"locations":[[1,0],[1,1],[1,2],[1,3]],"clue":"Instrument on Guinness logo","answer":"harp","direction":"across"},{"locations":[[1,5],[1,6],[1,7],[1,8]],"clue":"Nickname for Jose","answer":"pepe","direction":"across"},{"locations":[[2,0],[2,1],[2,2],[2,3]],"clue":"'Ahh, makes sense'","answer":"isee","direction":"across"},{"locations":[[2,5],[2,6],[2,7],[2,8]],"clue":"Et __ (and others)","answer":"alia","direction":"across"},{"locations":[[3,0],[3,1],[3,2],[3,3],[3,4]],"clue":"Two column notebook","answer":"steno","direction":"across"},{"locations":[[3,6],[3,7],[3,8]],"clue":"Suffix for auction and mountain","answer":"eer","direction":"across"},{"locations":[[4,2],[4,3],[4,4],[4,5],[4,6]],"clue":"Extemporize","answer":"adlib","direction":"across"},{"locations":[[5,0],[5,1],[5,2]],"clue":"Swing side to side","answer":"wag","direction":"across"},{"locations":[[5,4],[5,5],[5,6],[5,7],[5,8]],"clue":"Energy scandal from 2001","answer":"enron","direction":"across"},{"locations":[[6,0],[6,1],[6,2],[6,3]],"clue":"Scores perfectly","answer":"aces","direction":"across"},{"locations":[[6,5],[6,6],[6,7],[6,8]],"clue":"Famous lamp of the 1960's","answer":"lava","direction":"across"},{"locations":[[7,0],[7,1],[7,2],[7,3]],"clue":"Row on a chessboard","answer":"rank","direction":"across"},{"locations":[[7,5],[7,6],[7,7],[7,8]],"clue":"'Up and ___'","answer":"atem","direction":"across"},{"locations":[[8,0],[8,1],[8,2],[8,3]],"clue":"Regret or disappointment","answer":"pity","direction":"across"},{"locations":[[8,5],[8,6],[8,7],[8,8]],"clue":"Used to be","answer":"were","direction":"across"},{"locations":[[0,0],[1,0],[2,0],[3,0]],"clue":"Not that","answer":"this","direction":"down"},{"locations":[[5,0],[6,0],[7,0],[8,0]],"clue":"Drive for faster than light travel","answer":"warp","direction":"down"},{"locations":[[0,1],[1,1],[2,1],[3,1]],"clue":"Where hops are dried","answer":"oast","direction":"down"},{"locations":[[5,1],[6,1],[7,1],[8,1]],"clue":"Antioxidant rich superfood","answer":"acai","direction":"down"},{"locations":[[0,2],[1,2],[2,2],[3,2],[4,2],[5,2],[6,2],[7,2],[8,2]],"clue":"Professional athlete lacking a contract","answer":"freeagent","direction":"down"},{"locations":[[0,3],[1,3],[2,3],[3,3],[4,3]],"clue":"Turn over","answer":"upend","direction":"down"},{"locations":[[6,3],[7,3],[8,3]],"clue":"Lucy's locale","answer":"sky","direction":"down"},{"locations":[[3,4],[4,4],[5,4]],"clue":"Grand ___ Opry","answer":"ole","direction":"down"},{"locations":[[0,5],[1,5],[2,5]],"clue":"Bar order, briefly","answer":"ipa","direction":"down"},{"locations":[[4,5],[5,5],[6,5],[7,5],[8,5]],"clue":"Marital kinship","answer":"inlaw","direction":"down"},{"locations":[[0,6],[1,6],[2,6],[3,6],[4,6],[5,6],[6,6],[7,6],[8,6]],"clue":"Kool & the Gang album from 1980","answer":"celebrate","direction":"down"},{"locations":[[0,7],[1,7],[2,7],[3,7]],"clue":"Ron Howard in The Andy Griffith Show","answer":"opie","direction":"down"},{"locations":[[5,7],[6,7],[7,7],[8,7]],"clue":"End of message on the radio","answer":"over","direction":"down"},{"locations":[[0,8],[1,8],[2,8],[3,8]],"clue":"Close","answer":"near","direction":"down"},{"locations":[[5,8],[6,8],[7,8],[8,8]],"clue":"Appoint (someone)","answer":"name","direction":"down"}]}
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
    , viewClues model Across
    , viewClues model Down
    -- , text (model |> activeAnswers |> toString)
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
    div [ style [("float", "left"), ("overflow", "hidden")] ]
      [ h2 [] [ text (toString direction) ]
      , ul [] (List.map (\c -> li [ style (if (isActiveClue c) then [("background-color", "red")] else []) ] [ text c.clue ]) clues)
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
