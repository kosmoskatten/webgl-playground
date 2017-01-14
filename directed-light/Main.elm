module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Light exposing (Light)
import Math.Matrix4 exposing (Mat4, makePerspective, makeLookAt)
import Math.Vector3 exposing (vec3)
import WebGL as WebGL


type alias Model =
    { projection : Mat4
    , camera : Mat4
    , light : Light
    }


type Msg
    = NoOp


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    ( { projection =
            makePerspective 45 (toFloat width / toFloat height) 0.01 100
      , camera = makeLookAt (vec3 1 1 -5) (vec3 0 0 0) (vec3 0 1 0)
      , light = Light.init
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.antialias
        , WebGL.alpha True
        , WebGL.clearColor (42 / 255) (82 / 255) (190 / 255) 1
        ]
        [ Attr.width width, Attr.height height ]
        [ Light.entity model.projection model.camera model.light
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


width : Int
width =
    800


height : Int
height =
    600
