module Square exposing (Vertex, floorAt)

{- Square is the main building block for floor, walls and ceilings. -}

import Math.Vector3 exposing (Vec3, vec3)


type alias Vertex =
    { position :
        Vec3
        -- The position for the vertex.
    }


floorAt : Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
floorAt x y z =
    [ ( Vertex (vec3 (x - 0.5) y (z + 0.5))
      , Vertex (vec3 (x + 0.5) y (z + 0.5))
      , Vertex (vec3 (x + 0.5) y (z - 0.5))
      )
    , ( Vertex (vec3 (x + 0.5) y (z - 0.5))
      , Vertex (vec3 (x - 0.5) y (z - 0.5))
      , Vertex (vec3 (x - 0.5) y (z + 0.5))
      )
    ]
