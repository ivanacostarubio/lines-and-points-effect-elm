module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as App
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Char exposing (fromCode)
import Task exposing (..)
import Basics.Extra exposing (..)
import Random exposing (..)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Window.Size
    , mouse : Point
    , keyboard : String
    , points : List Point
    }


type alias Point =
    { x : Int, y : Int }


initialModel : Model
initialModel =
    { windowSize = { height = 0, width = 0 }
    , mouse = { x = 0, y = 0 }
    , keyboard = ""
    , points = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, randomPoint )


randomPoint : Cmd Msg
randomPoint =
    Random.generate RandomPointMsg (Random.int 0 100)


initialWindowSize : Cmd Msg
initialWindowSize =
    Task.perform never WindMsg Window.size


type Msg
    = KeyMsg Keyboard.KeyCode
    | MouseMsg Mouse.Position
    | WindMsg Window.Size
    | RandomPointMsg Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( { model | mouse = position }, Cmd.none )

        KeyMsg code ->
            ( { model | keyboard = (fromCode code) |> toString }, Cmd.none )

        WindMsg windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        RandomPointMsg int ->
            case List.length model.points of
                100 ->
                    ( { model | points = List.append model.points [ { x = int, y = int } ] }, Cmd.none )

                _ ->
                    ( { model | points = List.append model.points [ { x = int, y = int } ] }, randomPoint )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMsg
        , Keyboard.presses KeyMsg
        , Window.resizes WindMsg
        ]


view : Model -> Html Msg
view model =
    div []
        [ Html.text (toString model) ]
