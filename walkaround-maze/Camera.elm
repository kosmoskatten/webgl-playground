module Camera
    exposing
        ( Camera
        , init
        , matrix
        , animate
        , keyDown
        , keyUp
        )

{- The camera is representing a person, moving around in the maze. -}

import Keyboard exposing (KeyCode)
import Math.Vector3 exposing (Vec3, vec3, add, getX, getY, getZ)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeRotate, transform)
import Time exposing (Time, inSeconds)


{- The representing state of the camera. -}


type alias Camera =
    { position :
        Vec3
        -- The current position of the camera.
    , angle :
        Float
        -- The angle (in degrees) in which the camera is heading.
    , headAdjustment :
        HeadAdjustment
        -- Tilt adjustment.
    , leftArrowDown :
        Bool
        -- Is the left arrow key pressed?
    , rightArrowDown :
        Bool
        -- Is the right arrow key pressed?
    , upArrowDown :
        Bool
        -- Is the up arrow key pressed?
    , downArrowDown :
        Bool
        -- Is the down arrow key pressed?
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



{- Animate the camera. I.e. take care of moving around. -}


animate : Time -> Camera -> Camera
animate time camera =
    let
        t =
            inSeconds time
    in
        animateBackward t <|
            animateForward t <|
                animateRightRotate t <|
                    animateLeftRotate t camera


animateLeftRotate : Float -> Camera -> Camera
animateLeftRotate t camera =
    if camera.leftArrowDown then
        { camera | angle = camera.angle + t * 180 }
    else
        camera


animateRightRotate : Float -> Camera -> Camera
animateRightRotate t camera =
    if camera.rightArrowDown then
        { camera | angle = camera.angle - t * 180 }
    else
        camera


animateForward : Float -> Camera -> Camera
animateForward t camera =
    if camera.upArrowDown then
        { camera | position = aheadOf camera.angle -t camera.position }
    else
        camera


animateBackward : Float -> Camera -> Camera
animateBackward t camera =
    if camera.downArrowDown then
        { camera | position = aheadOf camera.angle (t / 2) camera.position }
    else
        camera



{- Event got when a key is pressed. -}


keyDown : KeyCode -> Camera -> Camera
keyDown code camera =
    case code of
        33 ->
            pageUpDown camera

        34 ->
            pageDownDown camera

        36 ->
            homeDown camera

        37 ->
            leftArrowDown camera

        38 ->
            upArrowDown camera

        39 ->
            rightArrowDown camera

        40 ->
            downArrowDown camera

        _ ->
            camera



{- Event got when a key is released. -}


keyUp : KeyCode -> Camera -> Camera
keyUp code camera =
    case code of
        37 ->
            leftArrowUp camera

        38 ->
            upArrowUp camera

        39 ->
            rightArrowUp camera

        40 ->
            downArrowUp camera

        _ ->
            camera


leftArrowDown : Camera -> Camera
leftArrowDown camera =
    if not camera.leftArrowDown then
        { camera
            | angle = camera.angle + 2
            , leftArrowDown = True
        }
    else
        camera


leftArrowUp : Camera -> Camera
leftArrowUp camera =
    { camera | leftArrowDown = False }


rightArrowDown : Camera -> Camera
rightArrowDown camera =
    if not camera.rightArrowDown then
        { camera
            | angle = camera.angle - 2
            , rightArrowDown = True
        }
    else
        camera


rightArrowUp : Camera -> Camera
rightArrowUp camera =
    { camera | rightArrowDown = False }


upArrowDown : Camera -> Camera
upArrowDown camera =
    if not camera.upArrowDown then
        { camera
            | position = aheadOf camera.angle forwardStride camera.position
            , upArrowDown = True
        }
    else
        camera


upArrowUp : Camera -> Camera
upArrowUp camera =
    { camera | upArrowDown = False }


downArrowDown : Camera -> Camera
downArrowDown camera =
    if not camera.downArrowDown then
        { camera
            | position = aheadOf camera.angle backwardStride camera.position
            , downArrowDown = True
        }
    else
        camera


downArrowUp : Camera -> Camera
downArrowUp camera =
    { camera | downArrowDown = False }


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
