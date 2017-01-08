module Maze
    exposing
        ( Maze
        , init
        , render
        , ambientLightning
        , setAmbientLightning
        , diffuseLightning
        , setDiffuseLightning
        )

import Bitwise exposing (and, or)
import Math.Matrix4 exposing (Mat4, mul, identity)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Drawable(..), Renderable, Texture)
import Square
    exposing
        ( Vertex
        , floorAt
        , ceilingAt
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
    , mazeCeiling : Drawable Vertex
    , mazeCeilingTexture : Texture
    , roomFloor : Drawable Vertex
    , roomFloorTexture : Texture
    , outdoorWalls : Drawable Vertex
    , outdoorWallTexture : Texture
    , ambientLightning : Bool
    , ambientStrength : Float
    , ambientColor : Vec3
    , diffuseLightning : Bool
    }


type alias Class =
    Int


init : Texture -> Texture -> Texture -> Texture -> Texture -> Maze
init mazeFloorTexture mazeWallTexture mazeCeilingTexture roomFloorTexture outdoorWallTexture =
    { mazeFloor = mazeFloor
    , mazeFloorTexture = mazeFloorTexture
    , mazeWalls = mazeWalls
    , mazeWallTexture = mazeWallTexture
    , mazeCeiling = mazeCeiling
    , mazeCeilingTexture = mazeCeilingTexture
    , roomFloor = roomFloor
    , roomFloorTexture = roomFloorTexture
    , outdoorWalls = outdoorWalls
    , outdoorWallTexture = outdoorWallTexture
    , ambientLightning = True
    , ambientStrength = 0.05
    , ambientColor = vec3 1 1 1
    , diffuseLightning = True
    }


render : Mat4 -> Mat4 -> Vec3 -> Vec3 -> Maze -> List Renderable
render proj view walkerPos walkerColor maze =
    -- Nothing in the maze will be scaled, rotated nor moved. That's until
    -- there will be support for normal matrices in Matrix4.
    let
        model =
            Math.Matrix4.identity

        mvp =
            mul proj <| mul view model
    in
        [ -- Render the maze floor.
          WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.mazeFloor
            { mvp = mvp
            , model = model
            , ambientLightning = maze.ambientLightning
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = maze.diffuseLightning
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.mazeFloorTexture
            }
          -- Render the maze ceilings.
        , WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.mazeCeiling
            { mvp = mvp
            , model = model
            , ambientLightning = maze.ambientLightning
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = maze.diffuseLightning
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.mazeCeilingTexture
            }
          -- Render the maze walls.
        , WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.mazeWalls
            { mvp = mvp
            , model = model
            , ambientLightning = maze.ambientLightning
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = maze.diffuseLightning
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.mazeWallTexture
            }
          -- Render the room floor.
        , WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.roomFloor
            { mvp = mvp
            , model = model
            , ambientLightning = maze.ambientLightning
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = maze.diffuseLightning
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.roomFloorTexture
            }
          -- Render the outdoor walls. No lightning!
        , WebGL.render Square.vertexShader
            Square.fragmentShader
            maze.outdoorWalls
            { mvp = mvp
            , model = model
            , ambientLightning = False
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = False
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.outdoorWallTexture
            }
        ]



{- Get the ambient lightning on/off value. -}


ambientLightning : Maze -> Bool
ambientLightning maze =
    maze.ambientLightning



{- Set the ambient lightning on/off value. -}


setAmbientLightning : Bool -> Maze -> Maze
setAmbientLightning val maze =
    { maze | ambientLightning = val }



{- Get the diffuse lightning on/off value. -}


diffuseLightning : Maze -> Bool
diffuseLightning maze =
    maze.diffuseLightning



{- Set the diffuse lightning on/off value. -}


setDiffuseLightning : Bool -> Maze -> Maze
setDiffuseLightning val maze =
    { maze | diffuseLightning = val }



{- The hardcoded maze is looking like follows:

   M = maze area
   R = room area
   W = Outdoor wall

         -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10
      -10  0   0   0   0   0   0   0   W   0   0   0   0   0   0   0   0   0   0   0   0   0
      - 9  0   0   0   0   0   0   0   W   0   0   M   M   M   M   R   R   R   R   R   R   0
      - 8  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   0
      - 7  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   0
      - 6  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   0
      - 5  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   0
      - 4  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   0
      - 3  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
      - 2  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
      - 1  0   0   0   0   0   0   0   W   M   M   M   M   M   0   0   0   0   M   0   0   0
      +-0  0   0   0   0   0   0   0   M   M   0  (M)  0   M   M   M   M   M   M   0   0   0
        1  0   0   0   0   0   0   0   W   M   M   M   M   M   0   0   0   0   M   0   0   0
        2  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        3  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        4  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        5  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        6  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        7  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        8  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   0
        9  0   0   0   0   0   0   0   W   0   0   M   M   M   M   M   M   M   M   0   0   0
       10  0   0   0   0   0   0   0   W   0   0   0   0   0   0   0   0   0   0   0   0   0

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


olw : Int
olw =
    512


osw : Int
osw =
    1024


onw : Int
onw =
    2048


orw : Int
orw =
    4096


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
      ( 0, 0, -9, or nw <| or lw <| or mc mf )
    , ( 0, 0, -8, or rw <| or lw <| or mc mf )
    , ( 0, 0, -7, or rw <| or lw <| or mc mf )
    , ( 0, 0, -6, or rw <| or lw <| or mc mf )
    , ( 0, 0, -5, or rw <| or lw <| or mc mf )
    , ( 0, 0, -4, or rw <| or lw <| or mc mf )
    , ( 0, 0, -3, or rw <| or lw <| or mc mf )
    , ( 0, 0, -2, or mc <| or lw <| or mf rw )
    , ( 0, 0, -1, or mc mf )
    , ( 0, 0, 0, or mc <| or rw <| or mf lw )
    , ( 0, 0, 1, or mc mf )
    , ( 0, 0, 2, or mc <| or lw <| or mf rw )
    , ( 0, 0, 3, or lw <| or rw <| or mc mf )
    , ( 0, 0, 4, or lw <| or rw <| or mc mf )
    , ( 0, 0, 5, or lw <| or rw <| or mc mf )
    , ( 0, 0, 6, or lw <| or rw <| or mc mf )
    , ( 0, 0, 7, or lw <| or rw <| or mc mf )
    , ( 0, 0, 8, or lw <| or rw <| or mc mf )
    , ( 0, 0, 9, or lw <| or sw <| or mc mf )
      -- Inner loop, clockwise.
    , ( 1, 0, -1, or mc <| or sw <| or mf nw )
    , ( 2, 0, -1, or mc <| or nw <| or mf rw )
    , ( 2, 0, 0, or mc <| or mf lw )
    , ( 2, 0, 1, or mc <| or sw <| or mf rw )
    , ( 1, 0, 1, or mc <| or sw <| or mf nw )
    , ( -1, 0, 1, or mc <| or sw <| or mf nw )
    , ( -2, 0, 1, or mc <| or sw <| or mf lw )
    , ( -2, 0, 0, or mc <| or rw mf )
    , ( -2, 0, -1, or mc <| or nw <| or mf lw )
    , ( -1, 0, -1, or mc <| or sw <| or mf nw )
      -- Room.
    , ( 4, 0, -9, rf )
    , ( 5, 0, -9, rf )
    , ( 6, 0, -9, rf )
    , ( 7, 0, -9, rf )
    , ( 8, 0, -9, rf )
    , ( 9, 0, -9, rf )
    , ( 4, 0, -8, rf )
    , ( 5, 0, -8, rf )
    , ( 6, 0, -8, rf )
    , ( 7, 0, -8, rf )
    , ( 8, 0, -8, rf )
    , ( 9, 0, -8, rf )
    , ( 4, 0, -7, rf )
    , ( 5, 0, -7, rf )
    , ( 6, 0, -7, rf )
    , ( 7, 0, -7, rf )
    , ( 8, 0, -7, rf )
    , ( 9, 0, -7, rf )
    , ( 4, 0, -6, rf )
    , ( 5, 0, -6, rf )
    , ( 6, 0, -6, rf )
    , ( 7, 0, -6, rf )
    , ( 8, 0, -6, rf )
    , ( 9, 0, -6, rf )
    , ( 4, 0, -5, rf )
    , ( 5, 0, -5, rf )
    , ( 6, 0, -5, rf )
    , ( 7, 0, -5, rf )
    , ( 8, 0, -5, rf )
    , ( 9, 0, -5, rf )
    , ( 4, 0, -4, rf )
    , ( 5, 0, -4, rf )
    , ( 6, 0, -4, rf )
    , ( 7, 0, -4, rf )
    , ( 8, 0, -4, rf )
    , ( 9, 0, -4, rf )
      -- Corridor going south from room.
    , ( 7, 0, -3, or rw <| or lw <| or mf mc )
    , ( 7, 0, -2, or rw <| or lw <| or mf mc )
    , ( 7, 0, -1, or rw <| or lw <| or mf mc )
    , ( 7, 0, 0, or rw <| or mf mc )
    , ( 7, 0, 1, or rw <| or lw <| or mf mc )
    , ( 7, 0, 2, or rw <| or lw <| or mf mc )
    , ( 7, 0, 3, or rw <| or lw <| or mf mc )
    , ( 7, 0, 4, or rw <| or lw <| or mf mc )
    , ( 7, 0, 5, or rw <| or lw <| or mf mc )
    , ( 7, 0, 6, or rw <| or lw <| or mf mc )
    , ( 7, 0, 7, or rw <| or lw <| or mf mc )
    , ( 7, 0, 8, or rw <| or lw <| or mf mc )
    , ( 7, 0, 9, or rw <| or sw <| or mf mc )
      -- Connection between inner loop and the ^ corridor.
    , ( 3, 0, 0, or sw <| or nw <| or mf mc )
    , ( 4, 0, 0, or sw <| or nw <| or mf mc )
    , ( 5, 0, 0, or sw <| or nw <| or mf mc )
    , ( 6, 0, 0, or sw <| or nw <| or mf mc )
      -- Southmost connection of the corridors.
    , ( 1, 0, 9, or sw <| or nw <| or mf mc )
    , ( 2, 0, 9, or sw <| or nw <| or mf mc )
    , ( 3, 0, 9, or sw <| or nw <| or mf mc )
    , ( 4, 0, 9, or sw <| or nw <| or mf mc )
    , ( 5, 0, 9, or sw <| or nw <| or mf mc )
    , ( 6, 0, 9, or sw <| or nw <| or mf mc )
      -- Northmost connection of corridor and room.
    , ( 1, 0, -9, or sw <| or nw <| or mf mc )
    , ( 2, 0, -9, or sw <| or nw <| or mf mc )
    , ( 3, 0, -9, or sw <| or nw <| or mf mc )
      -- Outdoor walls.
    , ( -3, 0, -10, olw )
    , ( -3, 0, -9, olw )
    , ( -3, 0, -8, olw )
    , ( -3, 0, -7, olw )
    , ( -3, 0, -6, olw )
    , ( -3, 0, -5, olw )
    , ( -3, 0, -4, olw )
    , ( -3, 0, -3, olw )
    , ( -3, 0, -2, olw )
    , ( -3, 0, -1, or osw olw )
    , ( -3, 0, 1, or onw olw )
    , ( -3, 0, 2, olw )
    , ( -3, 0, 3, olw )
    , ( -3, 0, 4, olw )
    , ( -3, 0, 5, olw )
    , ( -3, 0, 6, olw )
    , ( -3, 0, 7, olw )
    , ( -3, 0, 8, olw )
    , ( -3, 0, 9, olw )
    , ( -3, 0, 10, olw )
      -- Connection to outdoor from the maze
    , ( -3, 0, 0, or mf mc )
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


mazeCeiling : Drawable Vertex
mazeCeiling =
    Triangle <|
        List.concat <|
            List.map (uncurry3 ceilingAt) <|
                filterClass mc maze


roomFloor : Drawable Vertex
roomFloor =
    Triangle <|
        List.concat <|
            List.map (uncurry3 floorAt) <|
                filterClass rf maze


outdoorWalls : Drawable Vertex
outdoorWalls =
    let
        leftWalls =
            List.concat <|
                List.map (uncurry3 leftWallAt) <|
                    filterClass olw maze

        rightWalls =
            List.concat <|
                List.map (uncurry3 rightWallAt) <|
                    filterClass orw maze

        northWalls =
            List.concat <|
                List.map (uncurry3 northWallAt) <|
                    filterClass onw maze

        southWalls =
            List.concat <|
                List.map (uncurry3 southWallAt) <|
                    filterClass osw maze
    in
        Triangle <| leftWalls ++ rightWalls ++ northWalls ++ southWalls
