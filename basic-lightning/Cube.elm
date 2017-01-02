module Cube exposing (triangles)

import Math.Vector3 exposing (Vec3, vec3)


{-| A Cube's triangles, exposed in local coordinates
-}
triangles : List ( Vec3, Vec3, Vec3 )
triangles =
    [ -- Back face.
      ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 0.5 -0.5 )
    , ( vec3 0.5 0.5 -0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5 )
      -- Front face.
    , ( vec3 -0.5 -0.5 0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5 )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 -0.5 0.5 )
      -- Left face.
    , ( vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5, vec3 -0.5 -0.5 -0.5 )
    , ( vec3 -0.5 -0.5 -0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 0.5 0.5 )
      -- Right face.
    , ( vec3 0.5 0.5 0.5, vec3 0.5 0.5 -0.5, vec3 0.5 -0.5 -0.5 )
    , ( vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5, vec3 0.5 0.5 0.5 )
      -- Bottom face.
    , ( vec3 -0.5 -0.5 -0.5, vec3 0.5 -0.5 -0.5, vec3 0.5 -0.5 0.5 )
    , ( vec3 0.5 -0.5 0.5, vec3 -0.5 -0.5 0.5, vec3 -0.5 -0.5 -0.5 )
      -- Top face.
    , ( vec3 -0.5 0.5 -0.5, vec3 0.5 0.5 -0.5, vec3 0.5 0.5 0.5 )
    , ( vec3 0.5 0.5 0.5, vec3 -0.5 0.5 0.5, vec3 -0.5 0.5 -0.5 )
    ]
