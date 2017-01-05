module Square exposing (Vertex, floorAt, fragmentShader, vertexShader)

{- Square is the main building block for floor, walls and ceilings. A
   Square can be textured, and can be lighted.
-}

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


type alias Vertex =
    { position :
        Vec3
        -- The position for the vertex.
    , texCoord :
        Vec2
        -- Texture coordinates.
    }



{- Generate a floor segment with center at x, y z. -}


floorAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
floorAt x y z =
    [ ( { position = vec3 (x - 0.5) y (z + 0.5), texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) y (z + 0.5), texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) y (z - 0.5), texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) y (z - 0.5), texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) y (z - 0.5), texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) y (z + 0.5), texCoord = vec2 0 0 }
      )
    ]


vertexShader :
    Shader
        { attr
            | position : Vec3
            , texCoord : Vec2
        }
        { unif | mvp : Mat4 }
        { vTexCoord : Vec2 }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvp;

varying vec2 vTexCoord;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vTexCoord = texCoord;
}
|]


fragmentShader : Shader {} unif { vTexCoord : Vec2 }
fragmentShader =
    [glsl|
precision mediump float;

varying vec2 vTexCoord;

void main(void)
{
    gl_FragColor = vec4(1.0, 0.5, 0.31, 1.0);
}
|]
