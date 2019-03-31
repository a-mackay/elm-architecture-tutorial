import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Random
import Tuple

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Die
    = One
    | Two
    | Three
    | Four
    | Five
    | Six

intToDie : Int -> Maybe Die
intToDie x = case x of
    1 -> Just One
    2 -> Just Two
    3 -> Just Three
    4 -> Just Four
    5 -> Just Five
    6 -> Just Six
    _ -> Nothing


type alias Model =
    { normalDie : Die
    , weightedDie : Die
    }

init : () -> (Model, Cmd Msg)
init _ = ({ normalDie = One, weightedDie = One}, Cmd.none)

type Msg
    = Roll
    | NewFaces (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Roll ->
        let fairInt = Random.int 1 6
            weightedInt : Random.Generator Int
            weightedInt = Random.weighted (80, 6) [(10, 5), (10, 4)]
            randomTupleGen = Random.map2 (\x y -> (x, y)) fairInt weightedInt
        in (model, Random.generate NewFaces randomTupleGen)

    NewFaces (normalInt, weightedInt) -> 
        let normalDie = withDefault One (intToDie normalInt)
            weightedDie = withDefault Six (intToDie weightedInt)
        in ({ normalDie = normalDie, weightedDie = weightedDie }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (dieString model.normalDie) ]
        , h1 [] [ text (dieString model.weightedDie) ]
        , img [ src (dieImageUrl model.normalDie), width 300, height 300 ] []
        , img [ src (dieImageUrl model.weightedDie), width 150, height 150 ] []
        , div [] []
        , button [ onClick Roll ] [ text "Roll" ]
        ]

dieString : Die -> String
dieString die = case die of
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"

dieImageUrl : Die -> String
dieImageUrl die = case die of
    One -> "https://openclipart.org/image/2400px/svg_to_png/220834/1434442078.png"
    Two -> "https://openclipart.org/image/2400px/svg_to_png/220835/1434442095.png"
    Three -> "https://openclipart.org/image/2400px/svg_to_png/220836/1434442108.png"
    Four -> "https://openclipart.org/image/2400px/svg_to_png/220837/1434442120.png"
    Five -> "https://openclipart.org/image/2400px/svg_to_png/220838/1434442131.png"
    Six -> "https://openclipart.org/image/2400px/svg_to_png/220839/1434442143.png"