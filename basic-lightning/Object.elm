module Object exposing (Object, make, render)

import Cube exposing (triangles, normals)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeRotate, makeTranslate)
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Object =
    { mesh :
        Drawable Vertex
        -- The mesh, with all the shader attributes.
    , objectColor :
        Vec3
        -- The color of the object (uniform to shader).
    , position :
        Vec3
        -- The model space position of the object.
    , rotation :
        Float
        -- The model space Y-axis rotation of the object.
    }


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }



{- Make a new Object. -}


make : Vec3 -> Vec3 -> Float -> Object
make objectColor position rotation =
    { mesh = meshFromTriangles
    , objectColor = objectColor
    , position = position
    , rotation = rotation
    }



{- Render the Object. -}


render : Mat4 -> Mat4 -> Vec3 -> Vec3 -> Float -> Object -> Renderable
render proj view lightPosition lightColor ambientStrength object =
    WebGL.render vertexShader
        fragmentShader
        object.mesh
        { proj = proj
        , view = view
        , model = modelMatrix object
        , objectColor = object.objectColor
        , lightPosition = lightPosition
        , lightColor = lightColor
        , ambientStrength = ambientStrength
        }



{- Create a mesh of Vertex, created from Cube's trianges and normals -}


meshFromTriangles : Drawable Vertex
meshFromTriangles =
    Triangle <|
        List.map2
            (\( p1, p2, p3 ) ( n1, n2, n3 ) ->
                ( Vertex p1 n1, Vertex p2 n2, Vertex p3 n3 )
            )
            triangles
            normals



{- Create the Object's model matrix. -}


modelMatrix : Object -> Mat4
modelMatrix object =
    let
        rotate =
            makeRotate object.rotation <| vec3 0 1 0

        trans =
            makeTranslate object.position
    in
        mul trans rotate



{- Object vertex shader. -}


vertexShader :
    Shader
        { attr
            | position : Vec3
            , normal : Vec3
        }
        { unif
            | proj : Mat4
            , view : Mat4
            , model : Mat4
        }
        {}
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 proj;
uniform mat4 view;
uniform mat4 model;

void main (void)
{
    gl_Position = proj * view * model * vec4(position, 1.0);
}
|]



{- Object fragment shader. -}


fragmentShader :
    Shader {}
        { unif
            | objectColor : Vec3
            , lightPosition : Vec3
            , lightColor : Vec3
            , ambientStrength : Float
        }
        {}
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 objectColor;
uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform float ambientStrength;

void main (void)
{
    gl_FragColor = vec4(objectColor, 1.0);
}
|]
