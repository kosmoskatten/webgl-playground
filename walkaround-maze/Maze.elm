module Maze exposing (Maze, init, render)

import Math.Matrix4 exposing (Mat4, mul, identity)
import WebGL exposing (Drawable(..), Renderable, Texture)
import Square exposing (Vertex, floorAt)


type alias Maze =
    { mazeFloor : Drawable Vertex
    , mazeFloorTexture : Texture
    }


init : Texture -> Maze
init mazeFloorTexture =
    { mazeFloor = mazeFloor
    , mazeFloorTexture = mazeFloorTexture
    }


render : Mat4 -> Mat4 -> Maze -> List Renderable
render proj view maze =
    -- Nothing in the maze will be scaled, rotated nor moved. That's until
    -- there will be support for normal matrices in Matrix4.
    let
        model =
            Math.Matrix4.identity

        mvp =
            mul proj <| mul view model
    in
        [ WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.mazeFloor
            { mvp = mvp
            , texture = maze.mazeFloorTexture
            }
        ]



{- The hardcoded maze is looking like follows:

   M = maze area
   R = room area

         -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10
      -10  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 9  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 8  0   0   0   M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 7  0   0   0   M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 6  0   0   0   M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 5  0   0   0   M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 4  0   0   0   M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 3  0   M4  M5  M6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 2  0   M4  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 1  0   M4  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      +-0  0   M4  0   0   0   0   0   0   0   0  (M1) M7  M7  M7  M7  M7  M7  0   0   0   0
        1  0   M4  0   0   0   0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        2  0   M4  0   0   0   0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        3  0   M4  0   0   0   0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        4  0   M4  0   0   0   0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        5  0   M2  M2  M2  M2  M2  M2  M2  M2  M2  M1  0   0   0   0   0   0   0   0   0   0
        6  0   0   0   0   M3  0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        7  0   0   0   0   M3  0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        8  0   0   0   0   M3  0   0   0   0   0   M1  0   0   0   0   0   0   0   0   0   0
        9  0   0   0   0   M3  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
       10  0   0   0   0   M3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0

-}


mazeFloor : Drawable Vertex
mazeFloor =
    Triangle <|
        List.concat
            [ floorAt 0 0 0
            , floorAt 0 0 1
            , floorAt 0 0 2
            , floorAt 0 0 3
            , floorAt 0 0 4
            , floorAt 0 0 5
            , floorAt 0 0 6
            , floorAt 0 0 7
            , floorAt 0 0 8
              -- M2 starts
            , floorAt -1 0 5
            , floorAt -2 0 5
            , floorAt -3 0 5
            , floorAt -4 0 5
            , floorAt -5 0 5
            , floorAt -6 0 5
            , floorAt -7 0 5
            , floorAt -8 0 5
            , floorAt -9 0 5
              -- M3 starts
            , floorAt -6 0 5
            , floorAt -6 0 6
            , floorAt -6 0 7
            , floorAt -6 0 8
            , floorAt -6 0 9
            , floorAt -6 0 10
              --M4 starts
            , floorAt -9 0 5
            , floorAt -9 0 4
            , floorAt -9 0 3
            , floorAt -9 0 2
            , floorAt -9 0 1
            , floorAt -9 0 0
            , floorAt -9 0 -1
            , floorAt -9 0 -2
            , floorAt -9 0 -3
              -- M5 starts
            , floorAt -8 0 -3
              -- M6 starts
            , floorAt -7 0 -3
            , floorAt -7 0 -4
            , floorAt -7 0 -5
            , floorAt -7 0 -6
            , floorAt -7 0 -7
            , floorAt -7 0 -8
              --M7 starts
            , floorAt 1 0 0
            , floorAt 2 0 0
            , floorAt 3 0 0
            , floorAt 4 0 0
            , floorAt 5 0 0
            , floorAt 6 0 0
            ]
