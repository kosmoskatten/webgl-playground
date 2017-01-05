module Main exposing (main)

import AnimationFrame exposing (diffs)
import Camera exposing (Camera)
import Keyboard exposing (KeyCode, downs, ups)
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (vec3)
import Maze exposing (Maze)
import Html exposing (Html, div, h3, p, text)
import Html.Attributes as Attr
import Time exposing (Time)
import WebGL as WebGL


type alias Model =
    { projection : Mat4
    , camera : Camera
    , maze : Maze
    }


type Msg
    = Animate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { projection =
            makePerspective 45 (toFloat width / toFloat height) 0.01 100
      , camera = Camera.init (vec3 0 1 5) 0
      , maze = Maze.init
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ Attr.class "w3-row" ]
        [ div [ Attr.class "w3-col l1" ]
            [ p [] [] ]
        , div [ Attr.class "w3-col l10 w3-center" ]
            [ viewHeader model
            , view3DScene model
            ]
        , div [ Attr.class "w3-col l1" ]
            [ p [] [] ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Attr.class "w3-container w3-indigo" ]
        [ h3 []
            [ text "Elm WebGL Walkaround Maze Demo"
            ]
        ]


view3DScene : Model -> Html Msg
view3DScene model =
    div [ Attr.class "w3-container w3-black" ]
        [ WebGL.toHtml [ Attr.width width, Attr.height height ] <|
            Maze.render model.projection (Camera.matrix model.camera) model.maze
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            ( model
            , Cmd.none
            )

        KeyDown code ->
            ( { model | camera = Camera.keyDown code model.camera }
            , Cmd.none
            )

        KeyUp code ->
            ( { model | camera = Camera.keyUp code model.camera }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


width : Int
width =
    800


height : Int
height =
    600
