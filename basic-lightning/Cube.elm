module Cube exposing (triangles, normals)

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


normals : List ( Vec3, Vec3, Vec3 )
normals =
    [ back
    , back
    , front
    , front
    , left
    , left
    , right
    , right
    , bottom
    , bottom
    , top
    , top
    ]


back : ( Vec3, Vec3, Vec3 )
back =
    ( vec3 0 0 -1, vec3 0 0 -1, vec3 0 0 -1 )


front : ( Vec3, Vec3, Vec3 )
front =
    ( vec3 0 0 1, vec3 0 0 1, vec3 0 0 1 )


left : ( Vec3, Vec3, Vec3 )
left =
    ( vec3 -1 0 0, vec3 -1 0 0, vec3 -1 0 0 )


right : ( Vec3, Vec3, Vec3 )
right =
    ( vec3 1 0 0, vec3 1 0 0, vec3 1 0 0 )


bottom : ( Vec3, Vec3, Vec3 )
bottom =
    ( vec3 0 -1 0, vec3 0 -1 0, vec3 0 -1 0 )


top : ( Vec3, Vec3, Vec3 )
top =
    ( vec3 0 1 0, vec3 0 1 0, vec3 0 1 0 )
