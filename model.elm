module Model exposing (..)

import Array exposing (Array)
import List
import Maybe exposing (andThen)
import Dict exposing (Dict)

type alias Model =
  { size : Dimensions
  , grid : Grid
  , answers : List Answer
  , cursor : Position
  , direction : Direction
  , cellNumbers : Dict Position Int
  , message : Maybe String
  }


type alias Dimensions =
  { width : Int
  , height : Int
  }


type alias Answer =
  { locations : List Position
  , clue : String
  , answer : String
  , direction : Direction
  , number : Int
  }


type Cell
  = Value Char
  | Empty
  | Block


type Direction = Across | Down
type alias Position = (Int, Int)
type alias Grid = Array (Array Cell)


getCell : Position -> Grid -> Maybe Cell
getCell (row, col) grid =
  (Array.get row grid) `andThen` (Array.get col)


setCell : Position -> Cell -> Grid -> Grid
setCell (row, col) cell grid =
  case (Array.get row grid) of
    Just r -> Array.set row (Array.set col cell r) grid
    Nothing -> grid
