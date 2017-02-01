module Main exposing (main)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (width, height)
import Http as Http exposing (..)
import Math.Matrix4 exposing (Mat4, mul, makePerspective, makeLookAt, makeRotate, makeTranslate)
import Math.Vector3 exposing (Vec3, vec3, normalize)
import Objson exposing (Triangle, Vertex, decode)
import Shaders exposing (vertexShader, fragmentShader)
import Time exposing (Time, inSeconds)
import WebGL as GL exposing (Mesh, alpha, antialias, clearColor, depth, entity, triangles, toHtmlWith)


type alias Model =
    { projection : Mat4
    , view : Mat4
    , rotation : Float
    , sunRayDirection : Vec3
    , mesh : Result String (Mesh Vertex)
    }


type Msg
    = Animate Time
    | ModelLoaded (Result Error (List Triangle))



--| ModelLoaded


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
    ( { projection = makePerspective 45 (toFloat width / toFloat height) 0.1 100
      , view = makeLookAt (vec3 -2 2 7) (vec3 0 0 0) (vec3 0 1 0)
      , rotation = 0
      , sunRayDirection = normalize <| vec3 0 -1 0
      , mesh = Err "Loading ..."
      }
    , Http.send ModelLoaded <| Http.get "models/model.json" decode
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            let
                inS =
                    inSeconds t
            in
                ( { model | rotation = model.rotation + inS * (pi / 8) }, Cmd.none )

        ModelLoaded result ->
            case result of
                Ok obj ->
                    ( { model | mesh = Ok << GL.triangles <| List.map toGeometry obj }, Cmd.none )

                Err msg ->
                    ( { model | mesh = Err <| toString msg }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.mesh of
        Ok mesh ->
            renderModel mesh model

        Err msg ->
            div [] [ text msg ]


renderModel : Mesh Vertex -> Model -> Html Msg
renderModel mesh model =
    let
        m =
            makeRotate model.rotation <| vec3 0 1 0
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
                { mvp =
                    mul model.projection <| mul model.view m
                , model = m
                , sunRayDirection = model.sunRayDirection
                }
            ]


width : Int
width =
    800


height : Int
height =
    600


toGeometry : Triangle -> ( Vertex, Vertex, Vertex )
toGeometry t =
    ( t.vertex1, t.vertex2, t.vertex3 )
