module Decoders exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Array
import Dict exposing (Dict)


direction : Decoder Direction
direction =
  let
    specific s = case s of
      "across" -> map (always Across) string
      _ -> map (always Down) string
  in
    string `andThen` specific


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
  object5 Answer
    ("locations" := (list position))
    ("clue" := string)
    ("answer" := string)
    ("direction" := direction)
    ("number" := int)


model : Decoder Model
model =
  let
    blockGrid size = Array.repeat size.width Block |> Array.repeat size.height
    initialModel answers size =
      { size = size
      , grid = (blockGrid size)
      , cursor = (0, 0)
      , answers = answers |> List.sortBy .number
      , direction = Across
      , cellNumbers = cellNumbers answers
      , message = Nothing
      }
  in
    object2 initialModel
      ("answers" := (list answer))
      ("size" := dimensions)


cellNumbers : List Answer -> Dict Position Int
cellNumbers answers =
  let
    position answer = answer.locations |> List.head |> Maybe.withDefault (-1, -1)
  in
    List.map (\a -> (position a, a.number)) answers |> Dict.fromList
