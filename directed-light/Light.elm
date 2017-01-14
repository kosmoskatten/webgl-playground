module Light exposing (Light, init, entity)

import Math.Matrix4 exposing (Mat4, mul, makeScale, makeTranslate)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Entity, Shader)


type alias Light =
    { mesh : Mesh Vertex
    , position : Vec3
    , direction : Vec3
    , color : Vec3
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


init : Light
init =
    { mesh = light
    , position = vec3 0 0 1
    , direction = vec3 0 0 -1
    , color = yellow
    }


entity : Mat4 -> Mat4 -> Light -> Entity
entity proj view light =
    let
        mvp =
            mul proj <| mul view <| modelMatrix light
    in
        WebGL.entity vertexShader fragmentShader light.mesh { mvp = mvp }


modelMatrix : Light -> Mat4
modelMatrix light =
    let
        scale =
            makeScale <| vec3 0.2 0.2 0.2

        trans =
            makeTranslate light.direction
    in
        mul trans scale


light : Mesh Vertex
light =
    WebGL.triangles <|
        List.map
            (\( p1, p2, p3, c ) ->
                ( Vertex p1 c, Vertex p2 c, Vertex p3 c )
            )
            vertices


vertices : List ( Vec3, Vec3, Vec3, Vec3 )
vertices =
    [ -- Back face, which is having the light.
      ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 0.5 -0.5, yellow )
    , ( vec3 0.5 0.5 -0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5, yellow )
      -- Front face.
    , ( vec3 -0.5 -0.5 0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5, blue )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 -0.5 0.5, blue )
      -- Left face.
    , ( vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5, blue )
    , ( vec3 -0.5 -0.5 -0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 0.5 0.5, blue )
      -- Right face.
    , ( vec3 0.5 0.5 0.5, vec3 0.5 0.5 -0.5, vec3 0.5 -0.5 -0.5, blue )
    , ( vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5, blue )
      -- Bottom face.
    , ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5, blue )
    , ( vec3 0.5 -0.5 0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 -0.5 -0.5, blue )
      -- Top face.
    , ( vec3 -0.5 0.5 -0.5, vec3 0.5 0.5 -0.5, vec3 0.5 0.5 0.5, blue )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5, blue )
    ]


blue : Vec3
blue =
    vec3 0 0 1


yellow : Vec3
yellow =
    vec3 1 (147 / 255) (41 / 255)


vertexShader :
    Shader
        { attr
            | position : Vec3
            , color : Vec3
        }
        { unif | mvp : Mat4 }
        { vColor : Vec3 }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 color;

uniform mat4 mvp;

varying vec3 vColor;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vColor = color;
}
|]


fragmentShader : Shader {} unif { vColor : Vec3 }
fragmentShader =
    [glsl|
precision mediump float;

varying vec3 vColor;

void main(void)
{
    gl_FragColor = vec4(vColor, 1.0);
}
|]
