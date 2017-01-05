module Camera
    exposing
        ( Camera
        , init
        , matrix
        , leftArrowDown
        , rightArrowDown
        , upArrowDown
        , downArrowDown
        , pageDownDown
        , pageUpDown
        , homeDown
        )

import Math.Vector3 exposing (Vec3, vec3, add, getX, getY, getZ)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeRotate, transform)


type alias Camera =
    { position : Vec3
    , angle : Float
    , headAdjustment : HeadAdjustment
    , leftArrowDown : Bool
    , rightArrowDown : Bool
    , upArrowDown : Bool
    , downArrowDown : Bool
    }


type HeadAdjustment
    = LookUp
    | LookStraight
    | LookDown



{- Initializing the camera, at the given position and the given view angle. -}


init : Vec3 -> Float -> Camera
init position angle =
    { position = position
    , angle = angle
    , headAdjustment = LookStraight
    , leftArrowDown = False
    , rightArrowDown = False
    , upArrowDown = False
    , downArrowDown = False
    }



{- Calculating the view matrix for the camera -}


matrix : Camera -> Mat4
matrix camera =
    let
        focus =
            headAdjustment camera.headAdjustment <|
                aheadOf camera.angle viewStride camera.position
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
        | position = aheadOf camera.angle forwardStride camera.position
        , upArrowDown = True
    }


downArrowDown : Camera -> Camera
downArrowDown camera =
    { camera
        | position = aheadOf camera.angle backwardStride camera.position
    }


pageDownDown : Camera -> Camera
pageDownDown camera =
    { camera
        | headAdjustment = LookDown
    }


pageUpDown : Camera -> Camera
pageUpDown camera =
    { camera
        | headAdjustment = LookUp
    }


homeDown : Camera -> Camera
homeDown camera =
    { camera
        | headAdjustment = LookStraight
    }



{- Give a new vector, which is ahead the current. Given the stride
   and the angle.
-}


aheadOf : Float -> Float -> Vec3 -> Vec3
aheadOf angle stride current =
    let
        oneAhead =
            vec3 0 0 stride

        rotation =
            makeRotate (degrees angle) (vec3 0 1 0)

        direction =
            transform rotation oneAhead
    in
        add current direction


viewStride : Float
viewStride =
    -1.0


forwardStride : Float
forwardStride =
    -0.1


backwardStride : Float
backwardStride =
    0.1


headAdjustment : HeadAdjustment -> Vec3 -> Vec3
headAdjustment adjustment vec =
    let
        x =
            getX vec

        y =
            getY vec

        z =
            getZ vec
    in
        case adjustment of
            LookStraight ->
                vec3 x y z

            LookUp ->
                vec3 x (y + 0.5) z

            LookDown ->
                vec3 x (y - 0.5) z


up : Vec3
up =
    vec3 0 1 0
