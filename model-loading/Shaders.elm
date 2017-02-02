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
        { unif
            | mvp : Mat4
            , model : Mat4
        }
        { vAmbientColor : Vec4
        , vNormal : Vec3
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 mvp;
uniform mat4 model;

varying vec4 vAmbientColor;
varying vec3 vNormal;

vec4 ambientColor()
{
  vec3 color = vec3(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0);
  return vec4(color * 0.1, 1.0);
}

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vAmbientColor = ambientColor();
    vNormal = vec3(model * vec4(normal, 0.0));
}
|]


fragmentShader : Shader {} { unif | sunRayDirection : Vec3 } { vAmbientColor : Vec4, vNormal : Vec3 }
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 sunRayDirection;

varying vec4 vAmbientColor;
varying vec3 vNormal;

vec4 diffuseColor()
{
  vec3 color = vec3(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0);
  float diff = dot(-sunRayDirection, normalize(vNormal));
  if (diff > 0.0)
  {
    return vec4(color * diff * 0.5, 1.0);
  }
  else
  {
    return vec4(0.0);
  }
}

void main(void)
{
    gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0) + vAmbientColor + diffuseColor();
}
|]