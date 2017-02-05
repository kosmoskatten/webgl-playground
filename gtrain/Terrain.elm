module Terrain exposing (Vertex, Terrain, make, toWireframe)

import Array as Array exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3, add, cross, sub, normalize)


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias Terrain =
    { vertices : List Vertex
    , indices : List ( Int, Int, Int )
    }


make : Int -> Terrain
make dimensions =
    let
        -- Generate x, z coordinates grid.
        coords =
            genCoord dimensions

        -- Generate vertices with 0 value normals.
        vertices =
            List.map (\p -> Vertex p (vec3 0 0 0)) <|
                genHeightMap heightGen coords

        -- Generate indices.
        indices =
            makeTriangleIndices dimensions
    in
        { vertices = calculateNormals vertices indices
        , indices = indices
        }


toWireframe : Terrain -> Maybe (List ( Vertex, Vertex ))
toWireframe terrain =
    let
        vertices =
            Array.fromList terrain.vertices
    in
        List.foldl
            (\mAcc l ->
                case mAcc of
                    Just acc ->
                        case l of
                            Just ll ->
                                Just (acc ++ ll)

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
            (Just [])
        <|
            List.map (toLines vertices) terrain.indices


toLines : Array Vertex -> ( Int, Int, Int ) -> Maybe (List ( Vertex, Vertex ))
toLines vertices ( i1, i2, i3 ) =
    Maybe.map3
        (\v1 v2 v3 ->
            [ ( v1, v2 )
            , ( v2, v3 )
            , ( v3, v1 )
            ]
        )
        (Array.get i1 vertices)
        (Array.get i2 vertices)
        (Array.get i3 vertices)



{- Generate a raw grid of x and z coordinates centered around (0, 0). The
   input value 'dimensions' is the number of main row and cols, where each will
   contain two triangles.
-}


genCoord : Int -> List ( Float, Float )
genCoord dimensions =
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



{- Take the raw coords and generate heights using the given generator function -}


genHeightMap : (( Float, Float ) -> Vec3) -> List ( Float, Float ) -> List Vec3
genHeightMap =
    List.map


heightGen : ( Float, Float ) -> Vec3
heightGen ( x, z ) =
    vec3 x (sin x * cos z) z


makeTriangleIndices : Int -> List ( Int, Int, Int )
makeTriangleIndices dimensions =
    List.concat <|
        List.map (triangulateSquare dimensions) <|
            genSquareGrid dimensions


triangulateSquare : Int -> ( Int, Int ) -> List ( Int, Int, Int )
triangulateSquare dimensions ( col, row ) =
    let
        index =
            row * (dimensions + 1) + col

        nextRowIndex =
            (row + 1) * (dimensions + 1) + col
    in
        [ ( index, nextRowIndex, index + 1 )
        , ( nextRowIndex + 1, index + 1, nextRowIndex )
        ]


genSquareGrid : Int -> List ( Int, Int )
genSquareGrid dimensions =
    let
        cols =
            List.range 0 (dimensions - 1)
    in
        List.concat <|
            List.map
                (\col ->
                    let
                        row =
                            List.repeat dimensions col
                    in
                        zip cols row
                )
                cols


calculateNormals : List Vertex -> List ( Int, Int, Int ) -> List Vertex
calculateNormals vertices indices =
    Array.toList <|
        normalizeVertices <|
            calculateSurfaceNormals (Array.fromList vertices) indices


normalizeVertices : Array Vertex -> Array Vertex
normalizeVertices =
    Array.map (\v -> { v | normal = V3.normalize v.normal })


calculateSurfaceNormals : Array Vertex -> List ( Int, Int, Int ) -> Array Vertex
calculateSurfaceNormals vertices indices =
    List.foldl
        (\( i1, i2, i3 ) acc ->
            case calculateSurfaceNormal vertices ( i1, i2, i3 ) of
                Just normal ->
                    updateNormal i1 normal <|
                        updateNormal i2 normal <|
                            updateNormal i3 normal acc

                Nothing ->
                    acc
        )
        vertices
        indices


calculateSurfaceNormal : Array Vertex -> ( Int, Int, Int ) -> Maybe Vec3
calculateSurfaceNormal vertices ( i1, i2, i3 ) =
    Maybe.map3
        (\v1 v2 v3 ->
            let
                left =
                    V3.sub v2.position v1.position

                right =
                    V3.sub v3.position v1.position
            in
                V3.normalize <| V3.cross left right
        )
        (Array.get i1 vertices)
        (Array.get i2 vertices)
        (Array.get i3 vertices)


updateNormal : Int -> Vec3 -> Array Vertex -> Array Vertex
updateNormal index normal vertices =
    case Array.get index vertices of
        Just v ->
            Array.set index { v | normal = V3.add v.normal normal } vertices

        Nothing ->
            vertices


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
