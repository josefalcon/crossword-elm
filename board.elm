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
import Model exposing (..)
import Decoders exposing (..)
import Json.Decode exposing (decodeString)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


example = """
{"size":{"width":9,"height":9},"answers":[{"locations":[[0,0],[0,1],[0,2],[0,3]],"clue":"Vegetarian option","answer":"tofu","direction":"across","number":1},{"locations":[[0,0],[1,0],[2,0],[3,0]],"clue":"Not that","answer":"this","direction":"down","number":1},{"locations":[[0,1],[1,1],[2,1],[3,1]],"clue":"Where hops are dried","answer":"oast","direction":"down","number":2},{"locations":[[0,2],[1,2],[2,2],[3,2],[4,2],[5,2],[6,2],[7,2],[8,2]],"clue":"Professional athlete lacking a contract","answer":"freeagent","direction":"down","number":3},{"locations":[[0,3],[1,3],[2,3],[3,3],[4,3]],"clue":"Turn over","answer":"upend","direction":"down","number":4},{"locations":[[0,5],[0,6],[0,7],[0,8]],"clue":"Item on a desktop","answer":"icon","direction":"across","number":5},{"locations":[[0,5],[1,5],[2,5]],"clue":"Bar order, briefly","answer":"ipa","direction":"down","number":5},{"locations":[[0,6],[1,6],[2,6],[3,6],[4,6],[5,6],[6,6],[7,6],[8,6]],"clue":"Kool & the Gang album from 1980","answer":"celebrate","direction":"down","number":6},{"locations":[[0,7],[1,7],[2,7],[3,7]],"clue":"Ron Howard in The Andy Griffith Show","answer":"opie","direction":"down","number":7},{"locations":[[0,8],[1,8],[2,8],[3,8]],"clue":"Close","answer":"near","direction":"down","number":8},{"locations":[[1,0],[1,1],[1,2],[1,3]],"clue":"Instrument on Guinness logo","answer":"harp","direction":"across","number":9},{"locations":[[1,5],[1,6],[1,7],[1,8]],"clue":"Nickname for Jose","answer":"pepe","direction":"across","number":10},{"locations":[[2,0],[2,1],[2,2],[2,3]],"clue":"'Ahh, makes sense'","answer":"isee","direction":"across","number":11},{"locations":[[2,5],[2,6],[2,7],[2,8]],"clue":"Et __ (and others)","answer":"alia","direction":"across","number":12},{"locations":[[3,0],[3,1],[3,2],[3,3],[3,4]],"clue":"Two column notebook","answer":"steno","direction":"across","number":13},{"locations":[[3,4],[4,4],[5,4]],"clue":"Grand ___ Opry","answer":"ole","direction":"down","number":14},{"locations":[[3,6],[3,7],[3,8]],"clue":"Suffix for auction and mountain","answer":"eer","direction":"across","number":15},{"locations":[[4,2],[4,3],[4,4],[4,5],[4,6]],"clue":"Extemporize","answer":"adlib","direction":"across","number":16},{"locations":[[4,5],[5,5],[6,5],[7,5],[8,5]],"clue":"Marital kinship","answer":"inlaw","direction":"down","number":17},{"locations":[[5,0],[5,1],[5,2]],"clue":"Swing side to side","answer":"wag","direction":"across","number":18},{"locations":[[5,0],[6,0],[7,0],[8,0]],"clue":"Drive for faster than light travel","answer":"warp","direction":"down","number":18},{"locations":[[5,1],[6,1],[7,1],[8,1]],"clue":"Antioxidant rich superfood","answer":"acai","direction":"down","number":19},{"locations":[[5,4],[5,5],[5,6],[5,7],[5,8]],"clue":"Energy scandal from 2001","answer":"enron","direction":"across","number":20},{"locations":[[5,7],[6,7],[7,7],[8,7]],"clue":"End of message on the radio","answer":"over","direction":"down","number":21},{"locations":[[5,8],[6,8],[7,8],[8,8]],"clue":"Appoint (someone)","answer":"name","direction":"down","number":22},{"locations":[[6,0],[6,1],[6,2],[6,3]],"clue":"Scores perfectly","answer":"aces","direction":"across","number":23},{"locations":[[6,3],[7,3],[8,3]],"clue":"Lucy's locale","answer":"sky","direction":"down","number":24},{"locations":[[6,5],[6,6],[6,7],[6,8]],"clue":"Famous lamp of the 1960's","answer":"lava","direction":"across","number":25},{"locations":[[7,0],[7,1],[7,2],[7,3]],"clue":"Row on a chessboard","answer":"rank","direction":"across","number":26},{"locations":[[7,5],[7,6],[7,7],[7,8]],"clue":"'Up and ___'","answer":"atem","direction":"across","number":27},{"locations":[[8,0],[8,1],[8,2],[8,3]],"clue":"Regret or disappointment","answer":"pity","direction":"across","number":28},{"locations":[[8,5],[8,6],[8,7],[8,8]],"clue":"Used to be","answer":"were","direction":"across","number":29}]}
"""


init : (Model, Cmd Msg)
init =
  case (decodeString model example) of
    Err err ->
      Debug.crash err

    Ok model ->
      let
        clear position grid = setCell position Empty grid
        clearAnswer answer grid = List.foldl clear grid answer.locations
      in
        ({ model | grid = List.foldl clearAnswer model.grid model.answers }, Cmd.none)


getCursorCell : Model -> Cell
getCursorCell model =
  case (getCell model.cursor model.grid) of
    Just cell -> cell
    Nothing -> Debug.crash "Invalid state"


setCursorCell : Cell -> Model -> Grid
setCursorCell cell model =
  setCell model.cursor cell model.grid


-- update


type Msg
  = MoveCursor Int Int
  | SetCell Char
  | DeleteCell
  | ChangeDirection
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MoveCursor rowDelta colDelta ->
      let
        (row, col) = model.cursor
        rowBound v = (max 0 (min (model.size.width - 1) v))
        colBound v = (max 0 (min (model.size.height - 1) v))
      in
        ({ model | cursor = (rowBound (row + rowDelta), colBound (col + colDelta)) }, Cmd.none)

    SetCell c ->
      case (getCursorCell model) of
        Block -> (model, Cmd.none)
        _ ->
          let
            move = if (model.direction == Across) then MoveCursor 0 1 else MoveCursor 1 0
          in
            update move { model | grid = (setCursorCell (Value c) model) }

    DeleteCell ->
      case (getCursorCell model) of
        Block -> (model, Cmd.none)
        _ ->
          let
            move = if (model.direction == Across) then MoveCursor 0 -1 else MoveCursor -1 0
          in
            update move { model | grid = (setCursorCell Empty model) }

    ChangeDirection ->
      ({ model | direction = if (model.direction == Across) then Down else Across }, Cmd.none)

    _ -> (model, Cmd.none)


clueNumbers : Model -> Dict.Dict Position Int
clueNumbers model =
  (List.map (\a -> (a.locations |> List.head |> Maybe.withDefault (0, 0), a.number)) model.answers) |> Dict.fromList


activeAnswers : Model -> List Answer
activeAnswers model =
  let
    f = \a -> (List.any (\x -> model.cursor == x) a.locations)
  in
    List.filter f model.answers


view : Model -> Html Msg
view model =
  div [ style [ ("padding", "1rem") ] ]
    [ viewBoard model
    , viewClues model Across
    , viewClues model Down
    -- , text (model.direction |> toString)
    ]

viewBoard : Model -> Html Msg
viewBoard model =
  table [ style [("border-collapse", "collapse")] ]
    [ tbody [] (List.map (viewRow model) [0..(model.size.height - 1)]) ]


viewRow : Model -> Int -> Html Msg
viewRow model row =
  let
    -- activeClues = activeAnswers model
    -- number = if (List.any (\a -> a.locations |> head |> ((==) (row, col))) activeAnswers)
    --          then Just
    number col = (Dict.get (row, col) (clueNumbers model))
    cell col = case (getCell (row, col) model.grid) of
      Maybe.Just c -> viewCell c ((row, col) == model.cursor) (number col)
      Maybe.Nothing -> td [] []
  in
    tr [] (List.map cell [0..(model.size.width - 1)])


viewClues : Model -> Direction -> Html Msg
viewClues model direction =
  let
    activeClues = activeAnswers model
    isActiveClue c = List.any ((==) c) activeClues
    clues = model.answers |> List.filter (\s -> s.direction == direction) |> List.sortBy .number
  in
    div [ style [("float", "left"), ("overflow", "hidden")] ]
      [ h2 [] [ text (toString direction) ]
      , ul [ style [("padding", "0")] ] (List.map (\c -> viewClue c (isActiveClue c)) clues)
      ]


viewClue clue active =
  let
    listStyle = [ ("list-style", "none") ]
    highlight = if active then [("background-color", "red")] else []
  in
    li [ style (highlight ++ listStyle) ]
      [ text ((toString clue.number) ++ " ")
      , text clue.clue
      ]

letterBox : List (String, String)
letterBox =
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


backgroundColor : String -> List (String, String)
backgroundColor color = [ ("background-color", color) ]


numberStyle : List (String, String)
numberStyle =
  [ ("position", "absolute")
  , ("left", "0")
  , ("top", "0")
  , ("font-size", ".5rem")
  , ("line-height", "100%")
  ]


viewCell : Cell -> Bool -> Maybe Int -> Html Msg
viewCell model selected number =
  let
    maybeYellow = if selected then (backgroundColor "yellow") else []
    -- clicker = onClick Click
    clueNumber = case number of
      Maybe.Just n -> span [ style numberStyle ] [ n |> toString |> text ]
      Maybe.Nothing -> span [] []
  in
    case model of
      Empty -> td [ style (letterBox ++ maybeYellow) ] [ clueNumber ]
      Block -> td [ style (letterBox ++ (backgroundColor "black") ++ maybeYellow) ] [ ]
      Value c -> td [ style (letterBox ++ maybeYellow) ] [ clueNumber, text (String.fromChar c) ]


keyCodeToChar : Keyboard.KeyCode -> Msg
keyCodeToChar keyCode =
  case keyCode of
    32 -> ChangeDirection
    37 -> MoveCursor 0 -1
    38 -> MoveCursor -1 0
    39 -> MoveCursor 0 1
    40 -> MoveCursor 1 0
    8 -> DeleteCell
    _ -> if (keyCode >= 65 && keyCode <= 90)
         then keyCode |> Char.fromCode |> SetCell
         else NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs keyCodeToChar
