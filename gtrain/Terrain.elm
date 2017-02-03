module Terrain exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)


type Terrain
    = Terrain



{- Generate a raw grid of x and z coordinates centered around (0, 0). The
   input value 'dimensions' is the number of main row and cols, where each will
   contain two triangles.
-}


genRawGrid : Int -> List ( Float, Float )
genRawGrid dimensions =
    let
        offset =
            0.5 * (toFloat dimensions)

        xs =
            List.map (\x -> (toFloat x) - offset) <| List.range 0 dimensions
    in
        List.concat <|
            List.map
                (\x ->
                    let
                        zs =
                            List.repeat (dimensions + 1) x
                    in
                        zip xs zs
                )
                xs



{- Take the raw grid and generate heights using the given generator function -}


genHeightMap : (( Float, Float ) -> Vec3) -> List ( Float, Float ) -> List Vec3
genHeightMap =
    List.map


heightGen : ( Float, Float ) -> Vec3
heightGen ( x, z ) =
    vec3 x (sin x + cos z) z


triangulate : Int -> List ( Int, Int, Int )
triangulate dimensions =
    List.concat <|
        List.map (triangulateSquare dimensions) <|
            List.range 0 (dimensions * dimensions - 1)


triangulateSquare : Int -> Int -> List ( Int, Int, Int )
triangulateSquare dimensions index =
    let
        rowOffset =
            dimensions + 1
    in
        [ ( index, index + rowOffset, index + 1 )
        , ( index + 1, index + rowOffset, index + 1 + rowOffset )
        ]


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
