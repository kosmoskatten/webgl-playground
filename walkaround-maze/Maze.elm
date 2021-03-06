module Maze
    exposing
        ( Maze
        , init
        , entity
        , ambientLightning
        , setAmbientLightning
        , diffuseLightning
        , setDiffuseLightning
        )

import Bitwise exposing (and, or)
import Math.Matrix4 exposing (Mat4, mul, identity)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Entity, Texture)
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
    { mazeFloor : Mesh Vertex
    , mazeFloorTexture : Texture
    , mazeWalls : Mesh Vertex
    , mazeWallTexture : Texture
    , mazeCeiling : Mesh Vertex
    , mazeCeilingTexture : Texture
    , roomFloor : Mesh Vertex
    , roomFloorTexture : Texture
    , roomCeiling : Mesh Vertex
    , roomCeilingTexture : Texture
    , outdoorWalls : Mesh Vertex
    , outdoorWallTexture : Texture
    , outdoorGrass : Mesh Vertex
    , outdoorGrassTexture : Texture
    , ambientLightning : Bool
    , ambientStrength : Float
    , ambientColor : Vec3
    , diffuseLightning : Bool
    }


type alias Class =
    Int


init : Texture -> Texture -> Texture -> Texture -> Texture -> Texture -> Texture -> Maze
init mazeFloorTexture mazeWallTexture mazeCeilingTexture roomFloorTexture roomCeilingTexture outdoorWallTexture outdoorGrassTexture =
    { mazeFloor = mazeFloor
    , mazeFloorTexture = mazeFloorTexture
    , mazeWalls = mazeWalls
    , mazeWallTexture = mazeWallTexture
    , mazeCeiling = mazeCeiling
    , mazeCeilingTexture = mazeCeilingTexture
    , roomFloor = roomFloor
    , roomFloorTexture = roomFloorTexture
    , roomCeiling = roomCeiling
    , roomCeilingTexture = roomCeilingTexture
    , outdoorWalls = outdoorWalls
    , outdoorWallTexture = outdoorWallTexture
    , outdoorGrass = outdoorGrass
    , outdoorGrassTexture = outdoorGrassTexture
    , ambientLightning = True
    , ambientStrength = 0.05
    , ambientColor = vec3 1 1 1
    , diffuseLightning = True
    }


entity : Mat4 -> Mat4 -> Vec3 -> Vec3 -> Maze -> List Entity
entity proj view walkerPos walkerColor maze =
    -- Nothing in the maze will be scaled, rotated nor moved. That's until
    -- there will be support for normal matrices in Matrix4.
    let
        model =
            Math.Matrix4.identity

        mvp =
            mul proj <| mul view model
    in
        [ -- Render the maze floor.
          WebGL.entity Square.vertexShader
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
        , WebGL.entity Square.vertexShader
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
          -- Render the maze/room walls.
        , WebGL.entity Square.vertexShader
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
        , WebGL.entity Square.vertexShader
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
          -- Render the room ceiling.
        , WebGL.entity Square.vertexShader
            Square.fragmentShader
            maze.roomCeiling
            { mvp = mvp
            , model = model
            , ambientLightning = maze.ambientLightning
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = maze.diffuseLightning
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.roomCeilingTexture
            }
          -- Render the outdoor walls. No lightning!
        , WebGL.entity Square.vertexShader
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
          -- Render the outdoor grass. No lightning!
        , WebGL.entity Square.vertexShader
            Square.fragmentShader
            maze.outdoorGrass
            { mvp = mvp
            , model = model
            , ambientLightning = False
            , ambientStrength = maze.ambientStrength
            , ambientColor = maze.ambientColor
            , diffuseLightning = False
            , lightPosition = walkerPos
            , lightColor = walkerColor
            , texture = maze.outdoorGrassTexture
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
      -10  0   0   0   0   0   0   0   W   W   W   W   W   W   W   W   W   W   W   W   W   W
      - 9  0   0   0   0   0   0   0   W   0   0   M   M   M   M   R   R   R   R   R   R   W
      - 8  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   W
      - 7  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   W
      - 6  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   W
      - 5  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   W
      - 4  0   0   0   0   0   0   0   W   0   0   M   0   0   0   R   R   R   R   R   R   W
      - 3  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
      - 2  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
      - 1  0   0   0   0   0   0   0   W   M   M   M   M   M   0   0   0   0   M   0   0   W
      +-0  0   0   0   0   0   0   0   M   M   0  (M)  0   M   M   M   M   M   M   0   0   W
        1  0   0   0   0   0   0   0   W   M   M   M   M   M   0   0   0   0   M   0   0   W
        2  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        3  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        4  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        5  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        6  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        7  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        8  0   0   0   0   0   0   0   W   0   0   M   0   0   0   0   0   0   M   0   0   W
        9  0   0   0   0   0   0   0   W   0   0   M   M   M   M   M   M   M   M   0   0   W
       10  0   0   0   0   0   0   0   W   W   W   W   W   W   W   W   W   W   W   W   W   W

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


rc : Int
rc =
    128


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
    , ( 4, 0, -9, or rc <| or rf nw )
    , ( 5, 0, -9, or rc <| or rf nw )
    , ( 6, 0, -9, or rc <| or rf nw )
    , ( 7, 0, -9, or rc <| or rf nw )
    , ( 8, 0, -9, or rc <| or rf nw )
    , ( 9, 0, -9, or rc <| or rw <| or rf nw )
    , ( 4, 0, -8, or rc <| or rf lw )
    , ( 5, 0, -8, or rf rc )
    , ( 6, 0, -8, or rf rc )
    , ( 7, 0, -8, or rf rc )
    , ( 8, 0, -8, or rc rf )
    , ( 9, 0, -8, or rc <| or rf rw )
    , ( 4, 0, -7, or rc <| or rf lw )
    , ( 5, 0, -7, or rf rc )
    , ( 6, 0, -7, or rf rc )
    , ( 7, 0, -7, or rf rc )
    , ( 8, 0, -7, or rf rc )
    , ( 9, 0, -7, or rc <| or rf rw )
    , ( 4, 0, -6, or rc <| or rf lw )
    , ( 5, 0, -6, or rf rc )
    , ( 6, 0, -6, or rf rc )
    , ( 7, 0, -6, or rf rc )
    , ( 8, 0, -6, or rf rc )
    , ( 9, 0, -6, or rc <| or rf rw )
    , ( 4, 0, -5, or rc <| or rf lw )
    , ( 5, 0, -5, or rf rc )
    , ( 6, 0, -5, or rf rc )
    , ( 7, 0, -5, or rf rc )
    , ( 8, 0, -5, or rf rc )
    , ( 9, 0, -5, or rc <| or rf rw )
    , ( 4, 0, -4, or rc <| or sw <| or rf lw )
    , ( 5, 0, -4, or rc <| or rf sw )
    , ( 6, 0, -4, or rc <| or rf sw )
    , ( 7, 0, -4, or rf rc )
    , ( 8, 0, -4, or rc <| or rf sw )
    , ( 9, 0, -4, or rc <| or sw <| or rf rw )
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
    , ( -3, 0, -10, onw )
    , ( -2, 0, -10, onw )
    , ( -1, 0, -10, onw )
    , ( 0, 0, -10, onw )
    , ( 1, 0, -10, onw )
    , ( 2, 0, -10, onw )
    , ( 3, 0, -10, onw )
    , ( 4, 0, -10, onw )
    , ( 5, 0, -10, onw )
    , ( 6, 0, -10, onw )
    , ( 7, 0, -10, onw )
    , ( 8, 0, -10, onw )
    , ( 9, 0, -10, onw )
    , ( 10, 0, -10, onw )
    , ( 10, 0, -10, orw )
    , ( 10, 0, -9, orw )
    , ( 10, 0, -8, orw )
    , ( 10, 0, -7, orw )
    , ( 10, 0, -6, orw )
    , ( 10, 0, -5, orw )
    , ( 10, 0, -4, orw )
    , ( 10, 0, -3, orw )
    , ( 10, 0, -2, orw )
    , ( 10, 0, -1, orw )
    , ( 10, 0, 0, orw )
    , ( 10, 0, 1, orw )
    , ( 10, 0, 2, orw )
    , ( 10, 0, 3, orw )
    , ( 10, 0, 4, orw )
    , ( 10, 0, 5, orw )
    , ( 10, 0, 6, orw )
    , ( 10, 0, 7, orw )
    , ( 10, 0, 8, orw )
    , ( 10, 0, 9, orw )
    , ( 10, 0, 10, orw )
    , ( 10, 0, 10, osw )
    , ( 9, 0, 10, osw )
    , ( 8, 0, 10, osw )
    , ( 7, 0, 10, osw )
    , ( 6, 0, 10, osw )
    , ( 5, 0, 10, osw )
    , ( 4, 0, 10, osw )
    , ( 3, 0, 10, osw )
    , ( 2, 0, 10, osw )
    , ( 1, 0, 10, osw )
    , ( 0, 0, 10, osw )
    , ( -1, 0, 10, osw )
    , ( -2, 0, 10, osw )
    , ( -3, 0, 10, osw )
      -- Connection to outdoor from the maze
    , ( -3, 0, 0, or mf mc )
    ]


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 g ( a, b, c ) =
    g a b c


mazeFloor : Mesh Vertex
mazeFloor =
    WebGL.triangles <|
        List.concat <|
            List.map (uncurry3 floorAt) <|
                filterClass mf maze


mazeWalls : Mesh Vertex
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
        WebGL.triangles <| leftWalls ++ rightWalls ++ northWalls ++ southWalls


mazeCeiling : Mesh Vertex
mazeCeiling =
    WebGL.triangles <|
        List.concat <|
            List.map (uncurry3 ceilingAt) <|
                filterClass mc maze


roomFloor : Mesh Vertex
roomFloor =
    WebGL.triangles <|
        List.concat <|
            List.map (uncurry3 floorAt) <|
                filterClass rf maze


roomCeiling : Mesh Vertex
roomCeiling =
    WebGL.triangles <|
        List.concat <|
            List.map (uncurry3 ceilingAt) <|
                filterClass rc maze


outdoorWalls : Mesh Vertex
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
        WebGL.triangles <| leftWalls ++ rightWalls ++ northWalls ++ southWalls


outdoorGrass : Mesh Vertex
outdoorGrass =
    WebGL.triangles <|
        List.concat <|
            [ List.concat <| List.map (\x -> floorAt (toFloat x) 0 -10) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -9) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -8) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -7) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -6) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -5) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -4) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -3) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -2) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 -1) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 0) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 1) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 2) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 3) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 4) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 5) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 6) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 7) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 8) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 9) <| List.range -20 -4
            , List.concat <| List.map (\x -> floorAt (toFloat x) 0 10) <| List.range -20 -4
            ]
