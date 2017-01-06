module Maze exposing (Maze, init, render)

import Bitwise exposing (and, or)
import Math.Matrix4 exposing (Mat4, mul, identity)
import WebGL exposing (Drawable(..), Renderable, Texture)
import Square
    exposing
        ( Vertex
        , floorAt
        , leftWallAt
        , rightWallAt
        , northWallAt
        , southWallAt
        )


type alias Maze =
    { mazeFloor : Drawable Vertex
    , mazeFloorTexture : Texture
    , mazeWalls : Drawable Vertex
    , mazeWallTexture : Texture
    }


type alias Class =
    Int


init : Texture -> Texture -> Maze
init mazeFloorTexture mazeWallTexture =
    { mazeFloor = mazeFloor
    , mazeFloorTexture = mazeFloorTexture
    , mazeWalls = mazeWalls
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
            maze.mazeWalls
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
      - 5  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
      - 4  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
      - 3  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
      - 2  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
      - 1  0   0   0   0   0   0   0   0   M   M   M   M   M   0   0   0   0   0   0   0   0
      +-0  0   0   0   0   0   0   0   0   M   0  (M)  0   M   0   0   0   0   0   0   0   0
        1  0   0   0   0   0   0   0   0   M   M   M   M   M   0   0   0   0   0   0   0   0
        2  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        3  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        4  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        5  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        6  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        7  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        8  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
        9  0   0   0   0   0   0   0   0   0   0   M   0   0   0   0   0   0   0   0   0   0
       10  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0

-}


mf : Int
mf =
    1


lw : Int
lw =
    2


rw : Int
rw =
    4


nw : Int
nw =
    8


sw : Int
sw =
    16


mc : Int
mc =
    32


rf : Int
rf =
    64


filterClass :
    Class
    -> List ( Float, Float, Float, Class )
    -> List ( Float, Float, Float )
filterClass spec xs =
    List.map (\( x, y, z, _ ) -> ( x, y, z )) <|
        List.filter (\( _, _, _, c ) -> and c spec == spec) xs


maze : List ( Float, Float, Float, Class )
maze =
    [ -- Inner north to south corridor.
      ( 0, 0, -5, mf )
    , ( 0, 0, -4, mf )
    , ( 0, 0, -3, mf )
    , ( 0, 0, -2, or lw <| or mf rw )
    , ( 0, 0, -1, mf )
    , ( 0, 0, 0, or rw <| or mf lw )
    , ( 0, 0, 1, mf )
    , ( 0, 0, 2, or lw <| or mf rw )
    , ( 0, 0, 3, mf )
    , ( 0, 0, 4, mf )
    , ( 0, 0, 5, mf )
    , ( 0, 0, 6, mf )
    , ( 0, 0, 7, mf )
    , ( 0, 0, 8, mf )
    , ( 0, 0, 9, mf )
      -- First loop, clockwise.
    , ( 1, 0, -1, or sw <| or mf nw )
    , ( 2, 0, -1, or nw <| or mf rw )
    , ( 2, 0, 0, or rw <| or mf lw )
    , ( 2, 0, 1, or sw <| or mf rw )
    , ( 1, 0, 1, or sw <| or mf nw )
    , ( -1, 0, 1, or sw <| or mf nw )
    , ( -2, 0, 1, or sw <| or mf lw )
    , ( -2, 0, 0, or rw <| or mf lw )
    , ( -2, 0, -1, or nw <| or mf lw )
    , ( -1, 0, -1, or sw <| or mf nw )
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


mazeWalls : Drawable Vertex
mazeWalls =
    let
        leftWalls =
            List.concat <|
                List.map (uncurry3 leftWallAt) <|
                    filterClass lw maze

        rightWalls =
            List.concat <|
                List.map (uncurry3 rightWallAt) <|
                    filterClass rw maze

        northWalls =
            List.concat <|
                List.map (uncurry3 northWallAt) <|
                    filterClass nw maze

        southWalls =
            List.concat <|
                List.map (uncurry3 southWallAt) <|
                    filterClass sw maze
    in
        Triangle <| leftWalls ++ rightWalls ++ northWalls ++ southWalls
