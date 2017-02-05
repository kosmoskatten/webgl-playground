module ShadersGtrain exposing (vertexShader, fragmentShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


vertexShader : Shader { position : Vec3, normal : Vec3 } { mvp : Mat4 } {}
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

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
    gl_FragColor = vec4(0.2, 0.3, 0.4, 1.0);
}
|]
