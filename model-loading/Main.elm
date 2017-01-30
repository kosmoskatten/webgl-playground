module Main exposing (main)

import AnimationFrame exposing (diffs)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (width, height)
import Math.Matrix4 exposing (Mat4, mul, makePerspective, makeLookAt, makeRotate, makeTranslate)
import Math.Vector3 exposing (Vec3, vec3)
import Shaders exposing (vertexShader, fragmentShader)
import Time exposing (Time, inSeconds)
import WebGL as GL exposing (Mesh, alpha, antialias, clearColor, depth, entity, indexedTriangles, toHtmlWith)


type alias Model =
    { projection : Mat4
    , view : Mat4
    , rotation : Float
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
    ( { projection = makePerspective 45 (toFloat width / toFloat height) 0.1 100
      , view = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
      , rotation = 0
      }
    , Cmd.none
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


view : Model -> Html Msg
view model =
    div [] [ text "Hej" ]



{- renderModel : MeshWith Vertex -> Model -> Html Msg
   renderModel mesh model =
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
               (GL.indexedTriangles mesh.vertices mesh.indices)
               { mvp = mul model.projection model.view }
           ]
-}


width : Int
width =
    800


height : Int
height =
    600
