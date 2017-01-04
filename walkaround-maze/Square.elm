module Square exposing (Vertex, floorAt, fragmentShader, vertexShader)

{- Square is the main building block for floor, walls and ceilings. -}

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


type alias Vertex =
    { position :
        Vec3
        -- The position for the vertex.
    }


floorAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
floorAt x y z =
    [ ( Vertex (vec3 (x - 0.5) y (z + 0.5))
      , Vertex (vec3 (x + 0.5) y (z + 0.5))
      , Vertex (vec3 (x + 0.5) y (z - 0.5))
      )
    , ( Vertex (vec3 (x + 0.5) y (z - 0.5))
      , Vertex (vec3 (x - 0.5) y (z - 0.5))
      , Vertex (vec3 (x - 0.5) y (z + 0.5))
      )
    ]


vertexShader : Shader { attr | position : Vec3 } { unif | mvp : Mat4 } {}
vertexShader =
    [glsl|
attribute vec3 position;

uniform mat4 mvp;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
}
|]


fragmentShader : Shader {} unif {}
fragmentShader =
    [glsl|
precision mediump float;

void main(void)
{
    gl_FragColor = vec4(1.0, 0.5, 0.31, 1.0);
}
|]
