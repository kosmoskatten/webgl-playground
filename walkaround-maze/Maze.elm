module Maze exposing (Maze, init, render, maze, filterClass)

import Bitwise exposing (and, or)
import Math.Matrix4 exposing (Mat4, mul, identity)
import WebGL exposing (Drawable(..), Renderable, Texture)
import Square exposing (Vertex, floorAt, leftWallAt)


type alias Maze =
    { mazeFloor : Drawable Vertex
    , mazeFloorTexture : Texture
    , mazeWall : Drawable Vertex
    , mazeWallTexture : Texture
    }


type alias Class =
    Int


init : Texture -> Texture -> Maze
init mazeFloorTexture mazeWallTexture =
    { mazeFloor = mazeFloor
    , mazeFloorTexture = mazeFloorTexture
    , mazeWall = mazeWall
    , mazeWallTexture = mazeWallTexture
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
        , WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.mazeWall
            { mvp = mvp
            , texture = maze.mazeWallTexture
            }
        ]



{- The hardcoded maze is looking like follows:

   M = maze area
   R = room area

         -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10
      -10  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 9  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 8  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 7  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 5  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 4  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 3  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 2  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 1  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      +-0  0   0   0   0   0   0   0   0   0   0  (M)  0   0   0   0   0   0   0   0   0   0
        1  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        2  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        3  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        4  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        5  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        6  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        7  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        8  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
        9  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
       10  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0

-}


mf : Int
mf =
    1


lw : Int
lw =
    2


filterClass :
    Class
    -> List ( Float, Float, Float, Class )
    -> List ( Float, Float, Float )
filterClass spec xs =
    List.map (\( x, y, z, _ ) -> ( x, y, z )) <|
        List.filter (\( _, _, _, c ) -> and c spec == spec) xs


maze : List ( Float, Float, Float, Class )
maze =
    [ ( 0, 0, 0, or mf lw )
    , ( 0, 0, 1, or mf lw )
    ]


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 g ( a, b, c ) =
    g a b c


mazeFloor : Drawable Vertex
mazeFloor =
    Triangle <|
        List.concat <|
            List.map (uncurry3 floorAt) <|
                filterClass mf maze


mazeWall : Drawable Vertex
mazeWall =
    Triangle <|
        List.concat <|
            List.map (uncurry3 leftWallAt) <|
                filterClass lw maze
