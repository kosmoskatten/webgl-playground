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
        { mvp : Mat4 }
        { vAmbientColor : Vec4 }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 mvp;

varying vec4 vAmbientColor;

vec4 ambientColor()
{
  return vec4(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0, 1.0) * 0.1;
}

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vAmbientColor = ambientColor();
}
|]


fragmentShader : Shader {} unif { vAmbientColor : Vec4 }
fragmentShader =
    [glsl|
precision mediump float;

varying vec4 vAmbientColor;

void main(void)
{
    gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0) + vAmbientColor;
}
|]
