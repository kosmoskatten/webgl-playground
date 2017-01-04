module Maze exposing (Maze, init, render)

import Math.Matrix4 exposing (Mat4, mul, identity)
import WebGL exposing (Drawable(..), Renderable)
import Square exposing (Vertex, floorAt)


type alias Maze =
    { crossFloor : Drawable Vertex
    }


init : Maze
init =
    { crossFloor = Triangle <| floorAt 0 0 0
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
            maze.crossFloor
            { mvp = mvp
            }
        ]
