module Cell exposing (Model(Value, Empty, Block), Msg(Click), init, update, view)

import Html exposing (Html, td, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String

type Model
  = Value Char
  | Empty
  | Block


init : Model
init = Empty


type Msg
  = Clear
  | Set Model
  | Click


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear ->
      (Empty, Cmd.none)

    Set cell ->
      (cell, Cmd.none)

    _ -> (model, Cmd.none)


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


backgroundColor : String -> List (String, String)
backgroundColor color = [ ("background-color", color) ]


view : Model -> Bool -> Html Msg
view model selected =
  let
    maybeYellow = if selected then (backgroundColor "yellow") else []
    clicker = onClick Click
  in
    case model of
      Empty -> td [ clicker, style (letterBox ++ maybeYellow) ] []
      Block -> td [ clicker, style (backgroundColor "black") ] []
      Value c -> td [ clicker, style (letterBox ++ maybeYellow) ] [ text (String.fromChar c) ]
