module Cell exposing (Model(Value, Empty, Block), Msg(Click), update, view)

import Html exposing (Html, td, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String

type Model
  = Value Char
  | Empty
  | Block


type Msg
  = Clear
  | Set Model
  | Click


update : Msg -> Model -> Model
update msg model =
  case msg of
    Clear -> Empty
    Set cell -> cell
    _ -> model


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


view : Model -> Bool -> Maybe Int -> Html Msg
view model selected number =
  let
    maybeYellow = if selected then (backgroundColor "yellow") else []
    clicker = onClick Click
    clueNumber = case number of
      Maybe.Just n -> span [ style numberStyle ] [ n |> toString |> text ]
      Maybe.Nothing -> span [] []
  in
    case model of
      Empty -> td [ clicker, style (letterBox ++ maybeYellow) ] [ clueNumber ]
      Block -> td [ clicker, style (letterBox ++ (backgroundColor "black") ++ maybeYellow) ] [ ]
      Value c -> td [ clicker, style (letterBox ++ maybeYellow) ] [ clueNumber, text (String.fromChar c) ]
