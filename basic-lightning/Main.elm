module Main exposing (main)

import AnimationFrame exposing (diffs)
import Lamp exposing (Lamp, make, render, animate, color, position)
import Object exposing (Object, make, render)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makePerspective, makeLookAt)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import WebGL exposing (..)
import Time exposing (Time)


type alias Model =
    { projection : Mat4
    , eyePosition : Vec3
    , eyeFocus : Vec3
    , ambientStrength : Float
    , lamp : Lamp
    , object : Object
    }


type Msg
    = Animate Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { projection = makePerspective 45 (toFloat width / toFloat height) 0.01 100
      , eyePosition = vec3 1.3 1.4 5
      , eyeFocus = vec3 0 0 0
      , ambientStrength = 0.25
      , lamp = Lamp.make lightYellow 0.2 2.5
      , object = Object.make coral (vec3 0 0 0) 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "WebGL" ]
        , view3DScene model
        ]


view3DScene : Model -> Html Msg
view3DScene model =
    let
        view =
            camera model

        lamp =
            model.lamp
    in
        WebGL.toHtml [ Attr.width width, Attr.height height ]
            [ Lamp.render model.projection view model.lamp
            , Object.render model.projection
                view
                (position lamp)
                (color lamp)
                model.ambientStrength
                model.object
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            ( { model | lamp = Lamp.animate t model.lamp }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Animate


lightYellow : Vec3
lightYellow =
    vec3 1 1 0.5


coral : Vec3
coral =
    vec3 1 0.5 0.31


width : Int
width =
    800


height : Int
height =
    600


camera : Model -> Mat4
camera model =
    makeLookAt model.eyePosition model.eyeFocus <| vec3 0 1 0
