module Lamp exposing (Lamp, make, render, color, position)

import Cube exposing (triangles)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeTranslate, makeScale)
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Lamp =
    { mesh :
        Drawable Vertex
        -- The mesh, with all the shader attributes.
    , lightColor :
        Vec3
        -- The color of the lamp (uniform to shader).
    , position :
        Vec3
        -- The model space position of the lamp.
    , scaling :
        Float
        -- The scaling of the lamp.
    }


type alias Vertex =
    { position : Vec3
    }



{- Make a new Lamp -}


make : Vec3 -> Vec3 -> Float -> Lamp
make lightColor position scaling =
    { mesh = meshFromTriangles
    , lightColor = lightColor
    , position = position
    , scaling = scaling
    }



{- Render the Lamp -}


render : Mat4 -> Mat4 -> Lamp -> Renderable
render proj view lamp =
    WebGL.render vertexShader
        fragmentShader
        lamp.mesh
        { proj = proj
        , view = view
        , model = modelMatrix lamp
        , lightColor = lamp.lightColor
        }


color : Lamp -> Vec3
color lamp =
    lamp.lightColor


position : Lamp -> Vec3
position lamp =
    lamp.position



{- Create a mesh of Vertex, created from the Cube's triangles. -}


meshFromTriangles : Drawable Vertex
meshFromTriangles =
    Triangle <|
        List.map (\( p1, p2, p3 ) -> ( Vertex p1, Vertex p2, Vertex p3 )) triangles



{- Create the Lamp's model matrix. -}


modelMatrix : Lamp -> Mat4
modelMatrix lamp =
    let
        scale =
            makeScale (vec3 lamp.scaling lamp.scaling lamp.scaling)

        trans =
            makeTranslate lamp.position
    in
        mul trans scale


vertexShader :
    Shader { attr | position : Vec3 }
        { unif
            | proj : Mat4
            , view : Mat4
            , model : Mat4
        }
        {}
vertexShader =
    [glsl|
attribute vec3 position;

uniform mat4 proj;
uniform mat4 view;
uniform mat4 model;

void main (void)
{
    gl_Position = proj * view * model * vec4(position, 1.0);
}
|]


fragmentShader : Shader {} { unif | lightColor : Vec3 } {}
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 lightColor;

void main (void)
{
    gl_FragColor = vec4(lightColor, 1);
}
|]
