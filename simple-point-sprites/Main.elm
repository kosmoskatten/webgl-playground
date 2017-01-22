module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makePerspective, makeLookAt)
import WebGL exposing (..)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.Blend as Blend


type alias Model =
    { eyePosition : Vec3
    , view : Mat4
    , proj : Mat4
    }


type Msg
    = NoOp


type alias Point =
    { position : Vec3
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { eyePosition = vec3 0 0 5
      , view = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
      , proj = makePerspective 45 (toFloat width / toFloat height) 0.001 100
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        mvp =
            mul model.proj model.view
    in
        WebGL.toHtmlWith
            [ WebGL.depth 1
            , WebGL.antialias
            , WebGL.alpha True
            , WebGL.clearColor 0 0 (102 / 255) 1
            ]
            [ Attr.width width, Attr.height height ]
        <|
            List.map (viewParticle mvp model.eyePosition)
                [ Point <| vec3 0 0 0
                , Point <| vec3 1 0 -10
                ]


viewParticle : Mat4 -> Vec3 -> Point -> Entity
viewParticle mvp eyePosition point =
    WebGL.entityWith [ DepthTest.always { write = True, near = 0, far = 0 }, Blend.add Blend.srcAlpha Blend.dstAlpha ]
        vertexShader
        fragmentShader
        (WebGL.points [ point ])
        { eyePosition = eyePosition
        , mvp = mvp
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


width : Int
width =
    800


height : Int
height =
    600


vertexShader :
    Shader { attr | position : Vec3 }
        { unif
            | eyePosition : Vec3
            , mvp : Mat4
        }
        {}
vertexShader =
    [glsl|
attribute vec3 position;

uniform vec3 eyePosition;
uniform mat4 mvp;

const float vista = 100.0;
const float maxSize = 100.0;

float calcPointSize(void)
{
    return max((1.0 - distance(eyePosition, position) / vista) * maxSize, 0.0);
}

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    gl_PointSize = calcPointSize();
}
|]


fragmentShader : Shader attr unif {}
fragmentShader =
    [glsl|
precision mediump float;

void main(void)
{
    vec2 centered = gl_PointCoord - vec2(0.5);
    float f = dot(centered, centered);

    const vec4 color1 = vec4(0.2, 0.0, 0.8, 0.2);
    const vec4 color2 = vec4(0.7, 0.7, 1.0, 0.3);

    if (f < 0.25)
    {
        gl_FragColor = mix(color1, color2, smoothstep(0.1, 0.25, f));
    }
    else
    {
        discard;
    }
}
|]
