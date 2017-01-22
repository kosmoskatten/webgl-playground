module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (..)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.Blend as Blend


type alias Model =
    Int


type Msg
    = NoOp


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
    ( 1, Cmd.none )


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.antialias
        , WebGL.alpha True
        , WebGL.clearColor (51 / 255) (102 / 255) (204 / 255) 1
        ]
        [ Attr.width width, Attr.height height ]
        [ WebGL.entityWith [ DepthTest.default, Blend.add Blend.srcAlpha Blend.dstAlpha ]
            vertexShader
            fragmentShader
            (WebGL.points
                [ { position = vec3 0.07 0 0.07 }
                , { position = vec3 -0.07 0 -0.07 }
                , { position = vec3 0 -0.07 0 }
                ]
            )
            {}
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


width : Int
width =
    800


height : Int
height =
    600


vertexShader : Shader { attr | position : Vec3 } {} {}
vertexShader =
    [glsl|
attribute vec3 position;

void main(void)
{
    gl_Position = vec4(position, 1.0);
    gl_PointSize = 100.0;
}
|]


fragmentShader : Shader attr {} {}
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
        //gl_FragColor = mix(color1, color2, f);
        //gl_FragColor = color2;
    }
    else
    {
        discard;
    }
}
|]
