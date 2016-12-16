module Main exposing (..)

import Keyboard
import Window
import Debug


-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
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
    }



-- UPDATE


step : ( Float, Keys ) -> Model -> Model
step ( dt, keys ) mario =
    mario
        |> gravity dt
        |> jump keys
        |> walk keys
        |> physics dt
        |> Debug.watch "mario"


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


display : ( Int, Int ) -> Model -> Element
display ( w', h' ) mario =
    let
        ( w, h ) =
            ( toFloat w', toFloat h' )

        verb =
            if mario.y > 0 then
                "jump"
            else if mario.vx /= 0 then
                "walk"
            else
                "stand"

        dir =
            case mario.dir of
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
        collage w'
            h'
            [ rect w h
                |> filled (rgb 174 238 238)
            , rect w 50
                |> filled (rgb 74 167 43)
                |> move ( 0, 24 - h / 2 )
            , marioImage
                |> toForm
                |> Debug.trace "mario"
                |> move ( mario.x, mario.y + groundY )
            ]



-- SIGNALS


main : Signal Element
main =
    lift2 display Window.dimensions (foldp step mario input)


input : Signal ( Float, Keys )
input =
    let
        delta =
            lift (\t -> t / 20) (fps 25)

        deltaArrows =
            lift2 (,) delta (Debug.watch "arrows" <~ Keyboard.arrows)
    in
        sampleOn delta deltaArrows
