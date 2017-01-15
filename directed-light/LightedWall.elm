module LightedWall exposing (LightedWall, init, entity)

import Light exposing (Light)
import Math.Matrix4 exposing (Mat4, mul, makeScale, makeTranslate)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Entity, Shader)


type alias LightedWall =
    { mesh : Mesh Vertex
    , position : Vec3
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


init : LightedWall
init =
    { mesh = lightedWall
    , position = vec3 0 0 -1
    }


entity : Mat4 -> Mat4 -> Light -> LightedWall -> Entity
entity proj view light lightedWall =
    let
        model =
            modelMatrix lightedWall

        mvp =
            mul proj <| mul view model
    in
        WebGL.entity
            vertexShader
            fragmentShader
            lightedWall.mesh
            { mvp = mvp
            , model = model
            , lightPosition = light.position
            , lightDirection = light.direction
            , lightColor = light.color
            }


modelMatrix : LightedWall -> Mat4
modelMatrix lightedWall =
    let
        scale =
            makeScale <| vec3 4 4 1

        trans =
            makeTranslate lightedWall.position
    in
        mul trans scale


lightedWall : Mesh Vertex
lightedWall =
    WebGL.triangles <|
        List.map
            (\( p1, p2, p3, c ) ->
                ( Vertex p1 c, Vertex p2 c, Vertex p3 c )
            )
            vertices


vertices : List ( Vec3, Vec3, Vec3, Vec3 )
vertices =
    [ ( vec3 -0.5 -0.5 0.0, vec3 0.5 -0.5 0.0, vec3 0.5 0.5 0.0, black )
    , ( vec3 0.5 0.5 0.0, vec3 -0.5 0.5 0.0, vec3 -0.5 -0.5 0.0, black )
    ]


black : Vec3
black =
    vec3 0 0 0


vertexShader :
    Shader
        { attr
            | position : Vec3
            , color : Vec3
        }
        { unif | mvp : Mat4, model : Mat4 }
        { vPosition : Vec3
        , vColor : Vec3
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 color;

uniform mat4 mvp;
uniform mat4 model;

varying vec3 vPosition;
varying vec3 vColor;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vPosition = vec3(model * vec4(position, 1.0));
    vColor = color;
}
|]


fragmentShader :
    Shader {}
        { unif
            | lightPosition : Vec3
            , lightDirection : Vec3
            , lightColor : Vec3
        }
        { vPosition : Vec3
        , vColor : Vec3
        }
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 lightPosition;
uniform vec3 lightDirection;
uniform vec3 lightColor;

varying vec3 vPosition;
varying vec3 vColor;

void main(void)
{
    vec3 lightPositionVec = normalize(vPosition - lightPosition);
    float diff = acos(dot(lightPositionVec, lightDirection));

    if (diff >= 0.0 && diff < 0.393)
    {
        gl_FragColor = vec4(vColor + lightColor, 1.0);
    }
    else
    {
        gl_FragColor = vec4(vColor, 1.0);
    }
}
|]
