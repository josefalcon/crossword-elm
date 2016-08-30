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
  | SetCursor Position
  | SetCell Cell
  | ChangeDirection
  | NextClue
  | Validate
  | ClearMessage
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
  case (msg, model.message) of
    (ClearMessage, Just _) -> ({ model | message = Nothing }, Cmd.none)
    (_, Just _) -> (model, Cmd.none)
    _ -> case msg of
    SetCursor (toRow, toCol) ->
      let
        { width, height } = model.size
        nextRow = toRow |> min (width - 1) |> max 0
        nextCol = toCol |> min (height - 1) |> max 0
      in
        ({ model | cursor = (nextRow, nextCol) }, Cmd.none)

    MoveCursor rowDelta colDelta ->
      let
        reposition (row, col) = (row + rowDelta, col + colDelta)

        findPosition pos =
          case (getCell pos model.grid) of
            Just Block -> findPosition (reposition pos)
            Just _ -> Just pos
            _ -> Nothing
      in
        case (findPosition (reposition model.cursor)) of
          Just pos -> ({ model | cursor = pos }, Cmd.none)
          _ -> (model, Cmd.none)


    SetCell c ->
      case (getCursorCell model) of
        Block -> (model, Cmd.none)
        _ ->
          let
            nextGrid = setCursorCell c model
            nextModel = { model | grid = nextGrid }
            nextCursor = case (c, model.direction) of
              (Empty, Across) -> left
              (Empty, Down) -> up
              (_, Across) -> right
              _ -> down
            isFilled =
              nextGrid |> Array.toList |> List.concatMap Array.toList |> List.all ((/=) Empty)
          in
            if (isFilled) then
              update Validate nextModel
            else
              update nextCursor nextModel

    ChangeDirection ->
      ({ model | direction = if (model.direction == Across) then Down else Across }, Cmd.none)

    NextClue ->
      let
        clueList = List.filter (\a -> a.direction == model.direction) model.answers
        isActive answer = answer.locations |> List.any ((==) model.cursor)
        activeClues = List.filter isActive clueList
      in
        case activeClues of
          clue :: [] ->
            case (dropWhile (\a -> a.number /= clue.number) clueList) of
              clue :: nextClue :: _ -> case (List.head nextClue.locations) of
                Just position -> update (SetCursor position) model
                _ -> (model, Cmd.none)
              _ -> (model, Cmd.none)
          _ -> (model, Cmd.none)

    Validate ->
      let
        assertLetter (loc, letter) = case (getCell loc model.grid) of
          Just (Value c) -> (c |> String.fromChar |> String.toLower) == letter
          _ -> False
        locationsWithLetter answer = answer.answer |> String.split "" |> List.map2 (,) answer.locations
        validateAnswer answer = (locationsWithLetter answer) |> List.all assertLetter
      in
        let
          message = if (List.all validateAnswer model.answers) then
            "Correct"
          else
            "At least one letter is wrong"
        in
          ({ model | message = Just message }, Cmd.none)

    _ -> (model, Cmd.none)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred list =
  case list of
    x :: xs -> if (pred x) then dropWhile pred xs else list
    [] -> []


-- view


view : Model -> Html Msg
view model =
  let
    activeClues = List.filter (\a -> (a.locations |> List.any ((==) model.cursor))) model.answers
    maybeMessage = case model.message of
      Just m ->
        div []
          [ h3 [] [ text m ]
          , button [ onClick ClearMessage ] [ text "Ok"]
          ]
      Nothing -> div [] []
  in
    div [ style [ ("padding", "1rem"), ("display", "flex") ] ]
      [ maybeMessage
      , viewGrid model activeClues
      , viewClues model Across activeClues
      , viewClues model Down activeClues
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
    number col = (Dict.get (row, col) model.cellNumbers)

    activeCell col = List.any ((==) (row, col)) activeCells

    cell col = case (getCell (row, col) model.grid) of
      Just c -> viewCell c (row, col) ((row, col) == model.cursor) (activeCell col) (number col)
      Nothing -> td [] []
  in
    tr [] (List.map cell [0..(model.size.width - 1)])


viewCell : Cell -> Position -> Bool -> Bool -> Maybe Int -> Html Msg
viewCell model position selected active number =
  let
    background = if selected then (backgroundColor yellow)
      else if active then (backgroundColor blue)
      else []

    clicker = onClick (SetCursor position)

    clueNumber = case number of
      Just n -> span [ style numberStyle ] [ n |> toString |> text ]
      Nothing -> span [] []
  in
    case model of
      Empty ->
        td [ clicker, style (cellStyle ++ background) ] [ clueNumber ]

      Block ->
        td [ clicker, style (cellStyle ++ (backgroundColor "black") ++ background) ] []

      Value c ->
        td [ clicker, style (cellStyle ++ background) ] [ clueNumber, text (String.fromChar c) ]


viewClues : Model -> Direction -> List Answer -> Html Msg
viewClues model direction activeClues =
  let
    isActiveClue clue = List.any ((==) clue) activeClues
    clues = model.answers |> List.filter (\s -> s.direction == direction)
  in
    div [ style clueListStyle ]
      [ h2 [ style h2Style ] [ text (toString direction) ]
      , ul [ style [("padding", "0")] ] (List.map (\c -> viewClue c (isActiveClue c)) clues)
      ]


viewClue : Answer -> Bool -> Html Msg
viewClue clue active =
  let
    highlight = if active then (backgroundColor blue) else []
  in
    li [ style (clueStyle ++ highlight) ]
      [ text ((toString clue.number) ++ ". ")
      , text clue.clue
      ]


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
    13 -> NextClue -- TODO: this should be tab, but ugh, preventDefaults...
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


h2Style : List (String, String)
h2Style =
  [ ("margin-top", "0")
  ]


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


numberStyle : List (String, String)
numberStyle =
  [ ("position", "absolute")
  , ("left", "0")
  , ("top", "0")
  , ("font-size", ".5rem")
  , ("line-height", "100%")
  ]


clueListStyle : List (String, String)
clueListStyle =
  [ ("float", "left")
  , ("overflow", "hidden")
  , ("flex-basis", "100%")
  ]


clueStyle : List (String, String)
clueStyle =
  [ ("list-style", "none")
  ]
