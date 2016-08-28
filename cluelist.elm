module ClueList exposing (Model, view)


import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import List

type alias Model = List String

type Msg = None

view : Model -> Html Msg
view model =
  let
    clue c = li [] [ text c ]
  in
    div []
      [ ul [] (List.map clue model)
      ]
