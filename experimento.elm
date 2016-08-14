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
import Svg.Attributes exposing (..)
import Svg exposing (..)
import Time exposing (..)


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
    , velocities : List Point
    }


type alias Point =
    { x : Int, y : Int }


elementColor : String
elementColor =
    "#313131"


initialModel : Model
initialModel =
    { windowSize = { height = 0, width = 0 }
    , mouse = { x = 0, y = 0 }
    , keyboard = ""
    , points = []
    , velocities = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, initialWindowSize )


type Msg
    = KeyMsg Keyboard.KeyCode
    | MouseMsg Mouse.Position
    | WindMsg Window.Size
    | WindInitialMsg Window.Size
    | RandomPointMsg ( Int, Int )
    | RandomVelocityMsg ( Int, Int )
    | Tick Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMsg
        , Keyboard.presses KeyMsg
        , Window.resizes WindMsg
        , Time.every (80 * millisecond) Tick
        ]


numberOfPoints : Int
numberOfPoints =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( { model | mouse = position }, Cmd.none )

        KeyMsg code ->
            ( { model | keyboard = (fromCode code) |> toString }, Cmd.none )

        WindMsg windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        WindInitialMsg windowSize ->
            ( { model | windowSize = windowSize }, randomPoint model )

        RandomPointMsg ( a, b ) ->
            case List.length model.points of
                100 ->
                    ( { model | points = List.append model.points [ { x = a, y = b } ] }, randomVelocity model )

                _ ->
                    ( { model | points = List.append model.points [ { x = a, y = b } ] }, randomPoint model )

        RandomVelocityMsg ( a, b ) ->
            case List.length model.velocities of
                100 ->
                    ( { model | velocities = List.append model.velocities [ { x = a, y = b } ] }, Cmd.none )

                _ ->
                    ( { model | velocities = List.append model.velocities [ { x = a, y = b } ] }, randomVelocity model )

        Tick newTime ->
            ( animatePoints model, Cmd.none )


randomPoint : Model -> Cmd Msg
randomPoint model =
    Random.generate RandomPointMsg <| (Random.pair (Random.int 0 model.windowSize.width) (Random.int 0 model.windowSize.height))


randomVelocity : Model -> Cmd Msg
randomVelocity model =
    Random.generate RandomVelocityMsg <| (Random.pair (Random.int -1 1) (Random.int -1 1))


initialWindowSize : Cmd Msg
initialWindowSize =
    Task.perform never WindInitialMsg Window.size


animatePoints : Model -> Model
animatePoints model =
    let
        movePoint point velocity =
            { point | x = point.x + velocity.x, y = point.y + velocity.y }

        newPoints =
            List.map2 movePoint model.points model.velocities
    in
        { model | points = newPoints }


drawPoint : Point -> Svg msg
drawPoint point =
    let
        x =
            toString point.x

        y =
            toString point.y
    in
        Svg.circle [ cx x, cy y, r "1", fill elementColor ] []


allPoints : Model -> List (Svg b)
allPoints model =
    List.map drawPoint model.points


drawLine : Point -> Point -> Svg g
drawLine point mouse =
    let
        p =
            { x = toString point.x, y = toString point.y }

        m =
            { x = toString mouse.x, y = toString mouse.y }

        distance =
            sqrt (toFloat (point.x - mouse.x) ^ 2 + toFloat (point.y - mouse.y) ^ 2)

        color =
            if distance < 150.0 then
                elementColor
            else
                "fff"

        styles =
            Svg.Attributes.style ("stroke-width:0.3 ;stroke:" ++ color)
    in
        Svg.line [ x1 p.x, y1 p.y, x2 m.x, y2 m.y, styles ] []


allLines : Model -> List (Svg b)
allLines model =
    let
        mouseList =
            List.repeat (List.length model.points) model.mouse

        points =
            model.points
    in
        (List.map2 drawLine mouseList points)


view : Model -> Html Msg
view model =
    let
        windowHeight =
            toString model.windowSize.height

        windowWidth =
            toString model.windowSize.width

        lines =
            allLines model

        points =
            allPoints
    in
        Svg.svg [ viewBox ("0 0 " ++ windowWidth ++ " " ++ windowHeight) ] (List.append (allLines model) (allPoints model))
