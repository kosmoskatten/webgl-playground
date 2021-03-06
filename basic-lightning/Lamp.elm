module Lamp exposing (Lamp, make, render, animate, color, setColor, position)

import Cube exposing (triangles)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeTranslate, makeScale)
import Time exposing (Time, inSeconds)
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Lamp =
    { mesh :
        Drawable Vertex
        -- The mesh, with all the shader attributes.
    , lightColor :
        Vec3
        -- The color of the lamp (uniform to shader).
    , scaling :
        Float
        -- The scaling of the lamp.
    , distance :
        Float
        -- Distance from origin. To calculate orbit position.
    , theta :
        Float
        -- Angle to calculate orbit position.
    , position :
        Vec3
        -- The model space position of the lamp.
    }


type alias Vertex =
    { position : Vec3
    }



{- Make a new Lamp -}


make : Vec3 -> Float -> Float -> Lamp
make lightColor scaling distance =
    { mesh = meshFromTriangles
    , lightColor = lightColor
    , scaling = scaling
    , distance = distance
    , theta = 0
    , position = orbit 0 distance
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


animate : Time -> Lamp -> Lamp
animate t lamp =
    let
        duration =
            inSeconds t

        -- 1 / 8 rotation per second
        delta =
            duration * (pi / 4)

        theta =
            lamp.theta + delta

        position =
            orbit theta lamp.distance
    in
        { lamp | theta = theta, position = position }


color : Lamp -> Vec3
color lamp =
    lamp.lightColor


setColor : Vec3 -> Lamp -> Lamp
setColor color lamp =
    { lamp | lightColor = color }


position : Lamp -> Vec3
position lamp =
    lamp.position


orbit : Float -> Float -> Vec3
orbit theta distance =
    vec3 (distance * sin theta) 0 (distance * cos theta)



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



{- Lamp vertex shader. -}


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



{- Lamp fragment shader. -}


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
