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
        { vPosition : Vec4
        , vNormal : Vec3
        }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

uniform mat4 mvp;
uniform mat4 model;

varying vec4 vPosition;
varying vec3 vNormal;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vPosition = model * vec4(position, 1.0);
    vNormal = vec3(model * vec4(normal, 0.0));
}
|]


fragmentShader :
    Shader {}
        { unif | sunRayDirection : Vec3, eye : Vec3 }
        { vPosition : Vec4
        , vNormal : Vec3
        }
fragmentShader =
    [glsl|
precision mediump float;

uniform vec3 sunRayDirection;
uniform vec3 eye;

varying vec4 vPosition;
varying vec3 vNormal;

const vec3 color = vec3(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0);

/*vec4 diffuseColor()
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

vec4 speculatorColor()
{
  vec3 color = vec3(255.0 / 255.0, 241.0 / 255.0, 224.0 / 255.0);
  vec3 normal = normalize(vNormal);

  vec3 viewDir = normalize(eye - vPosition.xyz);
  vec3 reflectDir = normalize(reflect(-sunRayDirection, normal));
  float spec = pow(dot(viewDir, reflectDir), 32.0);

  if (spec > 0.0)
  {
    return vec4(color * spec, 1.0);
  }
  else
  {
    return vec4(0.0);
  }
}
*/

void main(void)
{
  vec4 ambientColor = vec4(color * 0.3, 1.0);
  vec4 diffuseColor = vec4(0.0);
  vec4 specularColor = vec4(0.0);

  vec3 normal = normalize(vNormal);

  // Calculate diffuse light.
  float diff = dot(-sunRayDirection, normal);
  if (diff > 0.0)
  {
    diffuseColor = vec4(color * diff * 0.5, 1.0);
  }

  // Calculate specular light.
  vec3 viewingVector = normalize(eye - vPosition.xyz);
  vec3 reflectVector = normalize(reflect(-sunRayDirection, normal));
  float spec = pow(dot(viewingVector, reflectVector), 32.0);
  if (spec > 0.0)
  {
    specularColor = vec4(color * spec * 0.8, 1.0);
  }

  gl_FragColor = vec4(0.1, 0.1, 0.1, 1.0) +
    ambientColor + diffuseColor + specularColor;
}
|]
