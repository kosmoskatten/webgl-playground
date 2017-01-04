module Camera exposing (Camera, init, matrix)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makeLookAt)


type alias Camera =
    { eye : Vec3
    , focus : Vec3
    , up : Vec3
    }



{- Initializing the camera, at the given position and the given focus. -}


init : Vec3 -> Vec3 -> Camera
init eye focus =
    { eye = eye
    , focus = focus
    , up = vec3 0 1 0
    }



{- Calculating the view matrix for the camera -}


matrix : Camera -> Mat4
matrix camera =
    makeLookAt camera.eye camera.focus camera.up
