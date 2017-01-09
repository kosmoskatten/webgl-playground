module LightCube exposing (LightCube, init, entity)

import Math.Matrix4 exposing (Mat4, mul)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Entity, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias LightCube =
    { mesh : Mesh Vertex
    , color : Vec3
    }


type alias Vertex =
    { position : Vec3
    }


init : Vec3 -> LightCube
init color =
    { mesh = lightCube
    , color = color
    }


entity : Mat4 -> Mat4 -> LightCube -> List Entity
entity proj view lightCube =
    let
        mvp =
            mul proj view
    in
        [ WebGL.entityWith [ DepthTest.default, Blend.add Blend.srcAlpha Blend.dstAlpha ]
            vertexShader
            fragmentShader
            lightCube.mesh
            { mvp = mvp, color = lightCube.color }
        ]


lightCube : Mesh Vertex
lightCube =
    WebGL.triangles <|
        List.map (\( p1, p2, p3 ) -> ( Vertex p1, Vertex p2, Vertex p3 )) triangles


triangles : List ( Vec3, Vec3, Vec3 )
triangles =
    [ -- Back face.
      ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 0.5 -0.5 )
    , ( vec3 0.5 0.5 -0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5 )
      -- Front face.
    , ( vec3 -0.5 -0.5 0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5 )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 -0.5 0.5 )
      -- Left face.
    , ( vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5 )
    , ( vec3 -0.5 -0.5 -0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 0.5 0.5 )
      -- Right face.
    , ( vec3 0.5 0.5 0.5, vec3 0.5 0.5 -0.5, vec3 0.5 -0.5 -0.5 )
    , ( vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5 )
      -- Bottom face.
    , ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5 )
    , ( vec3 0.5 -0.5 0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 -0.5 -0.5 )
      -- Top face.
    , ( vec3 -0.5 0.5 -0.5, vec3 0.5 0.5 -0.5, vec3 0.5 0.5 0.5 )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5 )
    ]


vertexShader : Shader { attr | position : Vec3 } { unif | mvp : Mat4 } {}
vertexShader =
    [glsl|
attribute vec3 position;

uniform mat4 mvp;

void main (void)
{
    gl_Position = mvp * vec4(position, 1);
}
|]


fragmentShader : Shader {} { unif | color : Vec3 } {}
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 color;

void main(void)
{
    gl_FragColor = vec4(color, 0.3);
}
|]
