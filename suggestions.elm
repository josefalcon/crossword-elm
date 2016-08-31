
import Html exposing (..)
import Html.Attributes exposing (href, target, value, type', checked)
import Html.App as App
import Html.Events exposing (..)
import Http
import Task
import Json.Decode exposing (Decoder, (:=), list, string, object2, maybe, map)
import Json.Encode
import VirtualDom

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\s -> Sub.none)
    }


type alias Hit =
  { title : String
  , url : String
  }


type alias Model =
  { pattern : String
  , suggestions : List Hit
  , size : Int
  , from : Int
  , hasNext : Bool
  , includeWikipedia : Bool
  }


init : (Model, Cmd Msg)
init =
  ({ pattern = "can"
  , suggestions = []
  , size = 25
  , from = 0
  , hasNext = False
  , includeWikipedia = False
  }, Cmd.none)


type Msg
  = NoOp
  | SetPattern String
  | LoadSuggestions
  | FetchSucceed ((List Hit), Bool)
  | FetchFail Http.Error
  | NextPage
  | PreviousPage
  | SetIncludeWikipedia Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetPattern p ->
      update LoadSuggestions { model | pattern = p }

    SetIncludeWikipedia w ->
      update LoadSuggestions { model | includeWikipedia = w }

    LoadSuggestions ->
      (model, loadSuggestions model)

    FetchSucceed (suggestions, hasNext) ->
      ({ model | suggestions = suggestions, hasNext = hasNext }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    NextPage ->
      update LoadSuggestions { model | from = model.from + model.size }

    PreviousPage ->
      update LoadSuggestions { model | from = model.from - model.size |> max 0 }

    _ ->
      (model, Cmd.none)


loadSuggestions : Model -> Cmd Msg
loadSuggestions model =
  let
    indices = "wiktionary" ++ if model.includeWikipedia then ",wikipedia" else ""
    url = Http.url
      ("http://192.241.158.90:8080/search/" ++ indices ++ "/" ++ model.pattern)
      [ ("size", model.size |> toString)
      , ("from", model.from |> toString)
      ]
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeSuggestions url)


decodeSuggestions : Decoder ((List Hit), Bool)
decodeSuggestions =
  object2 (,)
    ("hits" := list (object2 Hit ("title" := string) ("url" := string)))
    (map (\m -> m /= Nothing) (maybe ("nextPage" := string)))


innerHtml : String -> Attribute Msg
innerHtml =
  VirtualDom.property "innerHTML" << Json.Encode.string


view : Model -> Html Msg
view model =
  div []
    [ input [ onInput SetPattern, value model.pattern ] []
    , label []
        [ input [ type' "checkbox", checked model.includeWikipedia, onCheck SetIncludeWikipedia ] []
        , text "Wikipedia"
        ]
    , ul [] (List.map (\h -> li [] [ a [ href h.url, innerHtml h.title, target "_blank" ] [] ]) model.suggestions)
    , if model.from > 0 then button [ onClick PreviousPage ] [ text "Previous" ] else text ""
    , button [ onClick LoadSuggestions ] [ text "Load Suggestions" ]
    , if model.hasNext then button [ onClick NextPage ] [ text "Next" ] else text ""
    ]
