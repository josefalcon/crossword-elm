
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Http
import Task
import Json.Decode exposing (Decoder, (:=), list, string)
import Markdown

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\s -> Sub.none)
    }


type alias Model =
  { pattern : String
  , suggestions : List String
  }


init : (Model, Cmd Msg)
init =
  ({ pattern = "can"
  , suggestions = []
  }, Cmd.none)


type Msg
  = NoOp
  | Pattern String
  | LoadSuggestions
  | FetchSucceed (List String)
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pattern p ->
      ({ model | pattern = p }, Cmd.none)

    LoadSuggestions ->
      (model, loadSuggestions model.pattern)

    FetchSucceed suggestions ->
      ({ model | suggestions = suggestions }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    _ ->
      (model, Cmd.none)


loadSuggestions : String -> Cmd Msg
loadSuggestions pattern =
  let
    url = Http.url ("http://192.241.158.90:8080/search/wiktionary,wikipedia/" ++ pattern) [ ("size", "25") ]
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeSuggestions url)


decodeSuggestions : Decoder (List String)
decodeSuggestions =
  ("hits" := list ("title" := string))


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.pattern]
    , ul [] (List.map (\s -> li [] [(Markdown.toHtml [] s)]) model.suggestions)
    , button [ onClick LoadSuggestions ] [ text "Load Suggestions" ]
    ]
