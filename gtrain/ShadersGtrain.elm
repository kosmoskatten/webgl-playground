module ShadersGtrain exposing (vertexShader, fragmentShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


vertexShader :
    Shader
        { position : Vec3
        , normal : Vec3
        }
        { unif | mvp : Mat4 }
        { vPosition : Vec3
        , vNormal : Vec3
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 mvp;

varying vec3 vPosition;
varying vec3 vNormal;

void main(void)
{
    // Positions and normals are in model space atm. Just pass for
    // interpolation.
    vPosition = position;
    vNormal = normal;

    gl_Position = mvp * vec4(position, 1.0);
}
|]


fragmentShader :
    Shader {}
        { unif
            | lightPosition : Vec3
            , lightColor : Vec3
        }
        { vPosition : Vec3
        , vNormal : Vec3
        }
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 lightPosition;
uniform vec3 lightColor;

varying vec3 vPosition;
varying vec3 vNormal;

const float ambientStrength = 0.1;
const float diffuseFactor = 0.5;

void main(void)
{
    vec4 ambientColor = vec4(lightColor * ambientStrength, 1.0);

    vec3 normal = normalize(vNormal);
    vec3 lightDirection = normalize(lightPosition - vPosition);
    float diffuse = dot(lightDirection, normal);

    vec4 diffuseColor = vec4(0.0);
    if (diffuse > 0.0)
    {
        diffuseColor = vec4(lightColor * diffuse * diffuseFactor, 1.0);
    }

    gl_FragColor = vec4(0.0, 92.0 / 255.0, 0.9 / 255.0, 1.0) + ambientColor + diffuseColor;
}
|]
