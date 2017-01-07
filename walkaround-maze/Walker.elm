module Walker
    exposing
        ( Walker
        , init
        , matrix
        , position
        , animate
        , keyDown
        , keyUp
        )

{- The walker is representing a person, moving around in the maze. -}

import Keyboard exposing (KeyCode)
import Math.Vector3 exposing (Vec3, vec3, add, getX, getY, getZ)
import Math.Matrix4 exposing (Mat4, makeLookAt, makeRotate, transform)
import Time exposing (Time, inSeconds)


{- The representing state of the camera. -}


type alias Walker =
    { position :
        Vec3
        -- The current position of the walker.
    , angle :
        Float
        -- The angle (in degrees) in which the walker is heading.
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



{- Initializing the walker, at the given position and the given view angle. -}


init : Vec3 -> Float -> Walker
init position angle =
    { position = position
    , angle = angle
    , headAdjustment = LookStraight
    , leftArrowDown = False
    , rightArrowDown = False
    , upArrowDown = False
    , downArrowDown = False
    }



{- Calculating the view matrix for the walker/camera -}


matrix : Walker -> Mat4
matrix walker =
    let
        focus =
            headAdjustment walker.headAdjustment <|
                aheadOf walker.angle viewStride walker.position
    in
        makeLookAt walker.position focus up



{- Get the current position for the walker/camera. -}


position : Walker -> Vec3
position walker =
    walker.position



{- Animate the walker/camera. I.e. take care of moving around. -}


animate : Time -> Walker -> Walker
animate time walker =
    let
        t =
            inSeconds time
    in
        animateBackward t <|
            animateForward t <|
                animateRightRotate t <|
                    animateLeftRotate t walker


animateLeftRotate : Float -> Walker -> Walker
animateLeftRotate t walker =
    if walker.leftArrowDown then
        { walker | angle = walker.angle + t * 180 }
    else
        walker


animateRightRotate : Float -> Walker -> Walker
animateRightRotate t walker =
    if walker.rightArrowDown then
        { walker | angle = walker.angle - t * 180 }
    else
        walker


animateForward : Float -> Walker -> Walker
animateForward t walker =
    if walker.upArrowDown then
        { walker | position = aheadOf walker.angle -t walker.position }
    else
        walker


animateBackward : Float -> Walker -> Walker
animateBackward t walker =
    if walker.downArrowDown then
        { walker | position = aheadOf walker.angle (t / 2) walker.position }
    else
        walker



{- Event got when a key is pressed. -}


keyDown : KeyCode -> Walker -> Walker
keyDown code walker =
    case code of
        33 ->
            pageUpDown walker

        34 ->
            pageDownDown walker

        36 ->
            homeDown walker

        37 ->
            leftArrowDown walker

        38 ->
            upArrowDown walker

        39 ->
            rightArrowDown walker

        40 ->
            downArrowDown walker

        _ ->
            walker



{- Event got when a key is released. -}


keyUp : KeyCode -> Walker -> Walker
keyUp code walker =
    case code of
        37 ->
            leftArrowUp walker

        38 ->
            upArrowUp walker

        39 ->
            rightArrowUp walker

        40 ->
            downArrowUp walker

        _ ->
            walker


leftArrowDown : Walker -> Walker
leftArrowDown walker =
    if not walker.leftArrowDown then
        { walker
            | angle = walker.angle + 2
            , leftArrowDown = True
        }
    else
        walker


leftArrowUp : Walker -> Walker
leftArrowUp walker =
    { walker | leftArrowDown = False }


rightArrowDown : Walker -> Walker
rightArrowDown walker =
    if not walker.rightArrowDown then
        { walker
            | angle = walker.angle - 2
            , rightArrowDown = True
        }
    else
        walker


rightArrowUp : Walker -> Walker
rightArrowUp walker =
    { walker | rightArrowDown = False }


upArrowDown : Walker -> Walker
upArrowDown walker =
    if not walker.upArrowDown then
        { walker
            | position = aheadOf walker.angle forwardStride walker.position
            , upArrowDown = True
        }
    else
        walker


upArrowUp : Walker -> Walker
upArrowUp walker =
    { walker | upArrowDown = False }


downArrowDown : Walker -> Walker
downArrowDown walker =
    if not walker.downArrowDown then
        { walker
            | position = aheadOf walker.angle backwardStride walker.position
            , downArrowDown = True
        }
    else
        walker


downArrowUp : Walker -> Walker
downArrowUp walker =
    { walker | downArrowDown = False }


pageDownDown : Walker -> Walker
pageDownDown walker =
    { walker
        | headAdjustment = LookDown
    }


pageUpDown : Walker -> Walker
pageUpDown walker =
    { walker
        | headAdjustment = LookUp
    }


homeDown : Walker -> Walker
homeDown walker =
    { walker
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
