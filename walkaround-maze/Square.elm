module Square
    exposing
        ( Vertex
        , floorAt
        , ceilingAt
        , leftWallAt
        , rightWallAt
        , northWallAt
        , southWallAt
        , fragmentShader
        , vertexShader
        )

{- Square is the main building block for floor, walls and ceilings. A
   Square can be textured, and can be lighted.
-}

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader, Texture)


type alias Vertex =
    { position :
        Vec3
        -- The position for the vertex.
    , normal :
        Vec3
        -- The normal vector for the surface.
    , texCoord :
        Vec2
        -- Texture coordinates.
    }



{- Generate a floor segment with center at x, y, z. -}


floorAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
floorAt x y z =
    [ ( { position = vec3 (x - 0.5) y (z + 0.5), normal = up, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) y (z + 0.5), normal = up, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) y (z - 0.5), normal = up, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) y (z - 0.5), normal = up, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) y (z - 0.5), normal = up, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) y (z + 0.5), normal = up, texCoord = vec2 0 0 }
      )
    ]



{- Generate a ceiling segment with center at x, y, z. -}


ceilingAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
ceilingAt x y z =
    [ ( { position = vec3 (x - 0.5) (y + 2) (z + 0.5), normal = down, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) (y + 2) (z + 0.5), normal = down, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) (y + 2) (z - 0.5), normal = down, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) (y + 2) (z - 0.5), normal = down, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) (y + 2) (z - 0.5), normal = down, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) (y + 2) (z + 0.5), normal = down, texCoord = vec2 0 0 }
      )
    ]



{- Generate a wall segment on the left side of x, y, z. -}


leftWallAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
leftWallAt x y z =
    [ ( { position = vec3 (x - 0.5) y (z + 0.5), normal = right, texCoord = vec2 0 0 }
      , { position = vec3 (x - 0.5) y (z - 0.5), normal = right, texCoord = vec2 1 0 }
      , { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = right, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = right, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = right, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) y (z + 0.5), normal = right, texCoord = vec2 0 0 }
      )
    , ( { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = right, texCoord = vec2 0 0 }
      , { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = right, texCoord = vec2 1 0 }
      , { position = vec3 (x - 0.5) (y + 2) (z - 0.5), normal = right, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x - 0.5) (y + 2) (z - 0.5), normal = right, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) (y + 2) (z + 0.5), normal = right, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = right, texCoord = vec2 0 0 }
      )
    ]



{- Generate a wall segment on the right side of x, y, z. -}


rightWallAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
rightWallAt x y z =
    [ ( { position = vec3 (x + 0.5) y (z - 0.5), normal = left, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) y (z + 0.5), normal = left, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = left, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = left, texCoord = vec2 1 1 }
      , { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = left, texCoord = vec2 0 1 }
      , { position = vec3 (x + 0.5) y (z - 0.5), normal = left, texCoord = vec2 0 0 }
      )
    , ( { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = left, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = left, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) (y + 2) (z + 0.5), normal = left, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) (y + 2) (z + 0.5), normal = left, texCoord = vec2 1 1 }
      , { position = vec3 (x + 0.5) (y + 2) (z - 0.5), normal = left, texCoord = vec2 0 1 }
      , { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = left, texCoord = vec2 0 0 }
      )
    ]



{- Generate a wall segment on the north side of x, y, z. -}


northWallAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
northWallAt x y z =
    [ ( { position = vec3 (x - 0.5) y (z - 0.5), normal = backward, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) y (z - 0.5), normal = backward, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) y (z - 0.5), normal = backward, texCoord = vec2 0 0 }
      )
    , ( { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 0 0 }
      , { position = vec3 (x + 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 1 0 }
      , { position = vec3 (x + 0.5) (y + 2) (z - 0.5), normal = backward, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x + 0.5) (y + 2) (z - 0.5), normal = backward, texCoord = vec2 1 1 }
      , { position = vec3 (x - 0.5) (y + 2) (z - 0.5), normal = backward, texCoord = vec2 0 1 }
      , { position = vec3 (x - 0.5) (y + 1) (z - 0.5), normal = backward, texCoord = vec2 0 0 }
      )
    ]



{- Generate a wall segment on the south side of x, y, z. -}


southWallAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
southWallAt x y z =
    [ ( { position = vec3 (x + 0.5) y (z + 0.5), normal = forward, texCoord = vec2 0 0 }
      , { position = vec3 (x - 0.5) y (z + 0.5), normal = forward, texCoord = vec2 1 0 }
      , { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 1 1 }
      , { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 0 1 }
      , { position = vec3 (x + 0.5) y (z + 0.5), normal = forward, texCoord = vec2 0 0 }
      )
    , ( { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 0 0 }
      , { position = vec3 (x - 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 1 0 }
      , { position = vec3 (x - 0.5) (y + 2) (z + 0.5), normal = forward, texCoord = vec2 1 1 }
      )
    , ( { position = vec3 (x - 0.5) (y + 2) (z + 0.5), normal = forward, texCoord = vec2 1 1 }
      , { position = vec3 (x + 0.5) (y + 2) (z + 0.5), normal = forward, texCoord = vec2 0 1 }
      , { position = vec3 (x + 0.5) (y + 1) (z + 0.5), normal = forward, texCoord = vec2 0 0 }
      )
    ]


up : Vec3
up =
    vec3 0 1 0


down : Vec3
down =
    vec3 0 -1 0


left : Vec3
left =
    vec3 -1 0 0


right : Vec3
right =
    vec3 1 0 0


forward : Vec3
forward =
    vec3 0 0 -1


backward : Vec3
backward =
    vec3 0 0 1


vertexShader :
    Shader
        { attr
            | position : Vec3
            , normal : Vec3
            , texCoord : Vec2
        }
        { unif | mvp : Mat4, model : Mat4 }
        { vModelPosition : Vec3
        , vNormal : Vec3
        , vTexCoord : Vec2
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;
attribute vec2 texCoord;

uniform mat4 mvp;
uniform mat4 model;

varying vec3 vModelPosition;
varying vec3 vNormal;
varying vec2 vTexCoord;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);

    // Bring the position to model space (lightning is made in model space).
    vModelPosition = vec3(model * vec4(position, 1.0));

    // No real normal matrix yet. Just the model matrix, but the whole maze
    // is in local, which also is model, coordinates as nothing is scaled or
    // translated. Just use the normals as is.
    vNormal = normal;

    vTexCoord = texCoord;
}
|]


fragmentShader :
    Shader {}
        { unif
            | ambientLightning : Bool
            , ambientStrength : Float
            , ambientColor : Vec3
            , diffuseLightning : Bool
            , lightPosition : Vec3
            , lightColor : Vec3
            , lightDirection : Vec3
            , texture : Texture
        }
        { vModelPosition : Vec3
        , vNormal : Vec3
        , vTexCoord : Vec2
        }
fragmentShader =
    [glsl|
precision mediump float;

uniform bool ambientLightning;
uniform float ambientStrength;
uniform vec3 ambientColor;

uniform bool diffuseLightning;
uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 lightDirection;

uniform sampler2D texture;

varying vec3 vModelPosition;
varying vec3 vNormal;
varying vec2 vTexCoord;

vec3 maybeAddAmbientLight(vec3 inp)
{
    if (ambientLightning)
    {
        vec3 ambientCoeff = ambientColor * ambientStrength;
        return inp + ambientCoeff;
    }
    else
    {
        return inp;
    }
}

vec3 maybeAddDiffuseLight(vec3 inp)
{
    if (diffuseLightning)
    {
        vec3 lightDirectionT = normalize(lightPosition - vModelPosition);
        float coeff = max(dot(vNormal, lightDirectionT), 0.0);
        return inp + lightColor * coeff;
    }
    else
    {
        return inp;
    }
}

vec3 maybeAddDirectedLight(vec3 inp)
{
    if (diffuseLightning)
    {
        vec3 lightPositionVector = normalize(vModelPosition - lightPosition);
        float angle = acos(dot(lightPositionVector, lightDirection));
        if (angle >= 0.0 && angle < 0.1)
        {
            return inp + lightColor;
        }
        else
        {
            return inp;
        }
    }
    else
    {
        return inp;
    }
}

void main(void)
{
    if (ambientLightning || diffuseLightning)
    {
        // At least some lightning is activated.
        vec3 lightningCoeffs =
            maybeAddDirectedLight(
                maybeAddAmbientLight(vec3(0.0, 0.0, 0.0))
            );

        vec4 textureColor = texture2D(texture, vTexCoord);
        gl_FragColor = vec4(textureColor.rgb * lightningCoeffs, textureColor.a);
    }
    else
    {
        // No lightning at all.
        gl_FragColor = texture2D(texture, vTexCoord);
    }
}

|]
