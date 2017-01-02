module Main exposing (main)

import Lamp exposing (Lamp, make, render)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makePerspective, makeLookAt)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import WebGL exposing (..)


type alias Model =
    { projection : Mat4
    , eyePosition : Vec3
    , eyeFocus : Vec3
    , lamp : Lamp
    }


type Msg
    = NoOp


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
      , eyePosition = vec3 0 0 5
      , eyeFocus = vec3 0 0 0
      , lamp = Lamp.make lightYellow (vec3 1.2 0 0) 0.5
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
    WebGL.toHtml [ Attr.width width, Attr.height height ]
        [ Lamp.render model.projection (camera model) model.lamp
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


lightYellow : Vec3
lightYellow =
    vec3 1 1 0.5


width : Int
width =
    800


height : Int
height =
    600


camera : Model -> Mat4
camera model =
    makeLookAt model.eyePosition model.eyeFocus <| vec3 0 1 0
