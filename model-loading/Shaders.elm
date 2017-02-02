module Shaders exposing (vertexShader, fragmentShader)

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


vertexShader :
    Shader
        { position : Vec3
        , normal : Vec3
        }
        { mvp : Mat4
        , model : Mat4
        , sunRayDirection : Vec3
        }
        { vAmbientColor : Vec4
        , vDiffuseColor : Vec4
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 mvp;
uniform mat4 model;
uniform vec3 sunRayDirection;

varying vec4 vAmbientColor;
varying vec4 vDiffuseColor;

vec4 ambientColor()
{
  return vec4(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0, 1.0) * 0.1;
}

vec4 diffuseColor()
{
  vec3 rotatedNormal = normalize(vec3(model * vec4(normal, 0.0)));
  float diff = max(dot(-sunRayDirection, rotatedNormal), 0.0);
  return vec4(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0, 1.0) * diff;
}

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vAmbientColor = ambientColor();
    vDiffuseColor = diffuseColor();
}
|]


fragmentShader : Shader {} unif { vAmbientColor : Vec4, vDiffuseColor : Vec4 }
fragmentShader =
    [glsl|
precision mediump float;

varying vec4 vAmbientColor;
varying vec4 vDiffuseColor;

void main(void)
{
    gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0) + vAmbientColor + vDiffuseColor;
}
|]
