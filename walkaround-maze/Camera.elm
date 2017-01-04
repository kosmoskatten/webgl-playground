module Camera
    exposing
        ( Camera
        , init
        , matrix
        , leftArrowDown
        , rightArrowDown
        , upArrowDown
        )

import Math.Vector3 exposing (Vec3, vec3, add, getX, getY, getZ)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeRotate, transform)


type alias Camera =
    { position : Vec3
    , angle : Float
    , leftArrowDown : Bool
    , rightArrowDown : Bool
    , upArrowDown : Bool
    }



{- Initializing the camera, at the given position and the given view angle. -}


init : Vec3 -> Float -> Camera
init position angle =
    { position = position
    , angle = angle
    , leftArrowDown = False
    , rightArrowDown = False
    , upArrowDown = False
    }



{- Calculating the view matrix for the camera -}


matrix : Camera -> Mat4
matrix camera =
    let
        focus =
            oneStepAhead camera.angle camera.position
    in
        makeLookAt camera.position focus up



{- The left arrow key is pressed down. Make an initial turn without
   waiting for the next animate event.
-}


leftArrowDown : Camera -> Camera
leftArrowDown camera =
    { camera
        | angle = camera.angle + 5
        , leftArrowDown = True
    }


rightArrowDown : Camera -> Camera
rightArrowDown camera =
    { camera
        | angle = camera.angle - 5
        , rightArrowDown = True
    }


upArrowDown : Camera -> Camera
upArrowDown camera =
    { camera
        | position = oneStepAhead camera.angle camera.position
        , upArrowDown = True
    }



{- Take one step ahead in the given direction (in degrees). -}


oneStepAhead : Float -> Vec3 -> Vec3
oneStepAhead angle current =
    let
        oneAhead =
            vec3 0 0 stepUnit

        rotation =
            makeRotate (degrees angle) (vec3 0 1 0)

        direction =
            transform rotation oneAhead
    in
        add current direction


stepUnit : Float
stepUnit =
    -1.0


up : Vec3
up =
    vec3 0 1 0
