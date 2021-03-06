module Main exposing (main)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (width, height)
import Math.Matrix4
    exposing
        ( Mat4
        , mul
        , makePerspective
        , makeLookAt
        , makeRotate
        , transform
        )
import Math.Vector3 exposing (Vec3, vec3)
import ShadersGtrain exposing (vertexShader, fragmentShader)
import Terrain exposing (..)
import Time exposing (Time, inSeconds)
import WebGL as GL
    exposing
        ( Mesh
        , alpha
        , antialias
        , clearColor
        , depth
        , entity
        , lines
        , toHtmlWith
        )


type alias Model =
    { proj : Mat4
    , view : Mat4
    , terrain : Terrain
    , mesh : Maybe (Mesh Vertex)
    , lightPosition : Vec3
    , lightColor : Vec3
    }


type Msg
    = Animate Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> diffs Animate
        }


init : ( Model, Cmd Msg )
init =
    let
        terrain =
            make 30
    in
        ( { proj =
                makePerspective 45 (toFloat width / toFloat height) 0.01 1000
          , view = makeLookAt (vec3 0 10 30) (vec3 0 0 0) (vec3 0 1 0)
          , terrain =
                terrain
                --, mesh =
                --Maybe.map GL.lines <| toWireframe terrain
          , mesh = Just <| GL.indexedTriangles terrain.vertices terrain.indices
          , lightPosition =
                vec3 0 100 0
                -- Start at noon.
          , lightColor = vec3 (241 / 255) (241 / 255) (241 / 255)
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            let
                s =
                    inSeconds t

                rot =
                    makeRotate (s * (pi / 10)) <| vec3 0 0 1
            in
                ( { model | lightPosition = transform rot model.lightPosition }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.mesh of
        Just mesh ->
            viewMesh mesh model

        Nothing ->
            div [] [ text ":-(" ]


viewMesh : Mesh Vertex -> Model -> Html Msg
viewMesh mesh model =
    let
        mvp =
            mul model.proj model.view
    in
        GL.toHtmlWith
            [ GL.depth 1
            , GL.antialias
            , GL.alpha True
            , GL.clearColor 0 0 (102 / 255) 1
            ]
            [ Attr.width width
            , Attr.height height
            ]
            [ GL.entity vertexShader
                fragmentShader
                mesh
                { mvp = mvp
                , lightPosition = model.lightPosition
                , lightColor = model.lightColor
                }
            ]


width : Int
width =
    800


height : Int
height =
    600
