module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Task
import Time exposing (Time)
import Window


-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    , windowSize : Window.Size
    , keys : Keys
    }


type Direction
    = Left
    | Right


type alias Keys =
    { x : Int, y : Int }


mario : Model
mario =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , windowSize = { width = 0, height = 0 }
    , keys = { x = 0, y = 0 }
    }



-- UPDATE


applyKey : Int -> Keyboard.KeyCode -> Keys -> Keys
applyKey scale key keys =
    case key of
        37 ->
            { keys | x = -scale }

        38 ->
            { keys | y = scale }

        39 ->
            { keys | x = scale }

        40 ->
            { keys | y = -scale }

        _ ->
            keys


step : Msg -> Model -> ( Model, Cmd Msg )
step msg mario =
    case msg of
        Frame dt ->
            ( mario
                |> gravity (dt / 10)
                |> jump mario.keys
                |> walk mario.keys
                |> physics (dt / 10)
            , Cmd.none
            )

        KeyDown key ->
            ( { mario
                | keys = applyKey 1 key mario.keys
              }
            , Cmd.none
            )

        KeyUp key ->
            ( { mario | keys = applyKey 0 key mario.keys }
            , Cmd.none
            )

        WindowSize size ->
            ( { mario | windowSize = size }
            , Cmd.none
            )


jump : Keys -> Model -> Model
jump keys mario =
    if keys.y > 0 && mario.vy == 0 then
        { mario | vy = 6.0 }
    else
        mario


gravity : Float -> Model -> Model
gravity dt mario =
    { mario
        | vy =
            if mario.y > 0 then
                mario.vy - dt / 4
            else
                0
    }


physics : Float -> Model -> Model
physics dt mario =
    { mario
        | x = mario.x + dt * mario.vx
        , y = max 0 (mario.y + dt * mario.vy)
    }


walk : Keys -> Model -> Model
walk keys mario =
    { mario
        | vx = toFloat keys.x
        , dir =
            if keys.x < 0 then
                Left
            else if keys.x > 0 then
                Right
            else
                mario.dir
    }



-- DISPLAY


display : Model -> Html Msg
display model =
    let
        ( w, h ) =
            ( toFloat model.windowSize.width
            , toFloat model.windowSize.height
            )

        verb =
            if model.y > 0 then
                "jump"
            else if model.vx /= 0 then
                "walk"
            else
                "stand"

        dir =
            case model.dir of
                Left ->
                    "left"

                Right ->
                    "right"

        src =
            "imgs/mario/" ++ verb ++ "/" ++ dir ++ ".gif"

        marioImage =
            image 35 35 src

        groundY =
            62 - h / 2
    in
        collage
            model.windowSize.width
            model.windowSize.height
            [ rect w h
                |> filled (rgb 174 238 238)
            , rect w 50
                |> filled (rgb 74 167 43)
                |> move ( 0, 24 - h / 2 )
            , marioImage
                |> toForm
                |> move ( model.x, model.y + groundY )
            ]
            |> Element.toHtml



-- PROGRAM


type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size


main : Program Never Model Msg
main =
    Html.program
        { init = ( mario, Window.size |> Task.perform WindowSize )
        , update = step
        , view = display
        , subscriptions =
            \model ->
                Sub.batch
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs Frame
                    , Window.resizes WindowSize
                    ]
        }
