module Main exposing (main)

import AnimationFrame exposing (diffs)
import Camera exposing (Camera)
import Keyboard exposing (KeyCode, downs, ups)
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (vec3, getX, getZ)
import Maze exposing (Maze)
import Html exposing (Html, div, h3, p, span, text)
import Html.Attributes as Attr
import Html.Events as Evts
import Task exposing (attempt, sequence)
import Time exposing (Time, inSeconds)
import WebGL exposing (Texture)


type alias Model =
    { projection : Mat4
    , camera : Camera
    , maze : Maybe Maze
    , fps : Float
    , errStr : Maybe String
    }


type Msg
    = Animate Time
    | ClearErrorMessage
    | Error String
    | KeyDown KeyCode
    | KeyUp KeyCode
    | TexturesLoaded (List Texture)


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
      , camera = Camera.init (vec3 0 1.3 6) 0
      , maze = Nothing
      , fps = 0
      , errStr = Nothing
      }
    , loadTextures
        [ "textures/maze-floor.jpg"
        , "textures/maze-wall.jpg"
        ]
    )


view : Model -> Html Msg
view model =
    div [ Attr.class "w3-row" ]
        [ div [ Attr.class "w3-col l1" ]
            [ p [] [] ]
        , div [ Attr.class "w3-col l10 w3-center" ]
            [ viewHeader model
            , viewErrorMessage model
            , view3DScene model
            ]
        , div [ Attr.class "w3-col l1" ]
            [ p [] [] ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Attr.class "w3-container w3-indigo" ]
        [ h3 []
            [ text <|
                "Elm WebGL Walkaround Maze Demo [X = "
                    ++ toString (ceiling (getX model.camera.position))
                    ++ ", Y = "
                    ++ toString (ceiling (getZ model.camera.position))
                    ++ "] @ "
                    ++ toString (ceiling model.fps)
                    ++ " fps"
            ]
        ]


viewErrorMessage : Model -> Html Msg
viewErrorMessage model =
    case model.errStr of
        Just errStr ->
            div [ Attr.class "w3-container w3-red" ]
                [ span
                    [ Attr.class "w3-closebtn"
                    , Evts.onClick ClearErrorMessage
                    ]
                    [ text "X" ]
                , h3 [] [ text errStr ]
                ]

        Nothing ->
            div [] []


view3DScene : Model -> Html Msg
view3DScene model =
    div [ Attr.class "w3-container w3-black" ]
        [ WebGL.toHtml [ Attr.width width, Attr.height height ] <|
            case model.maze of
                Just theMaze ->
                    Maze.render model.projection
                        (Camera.matrix model.camera)
                        theMaze

                Nothing ->
                    []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            ( { model
                | camera = Camera.animate t model.camera
                , fps = updateFps t model.fps
              }
            , Cmd.none
            )

        ClearErrorMessage ->
            ( { model | errStr = Nothing }
            , Cmd.none
            )

        Error errStr ->
            ( { model | errStr = Just errStr }
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

        TexturesLoaded [ mazeFloorTexture, mazeWallTexture ] ->
            ( { model
                | maze =
                    Just (Maze.init mazeFloorTexture mazeWallTexture)
              }
            , Cmd.none
            )

        TexturesLoaded _ ->
            ( { model | errStr = Just "Unexpected number of textures loaded" }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


loadTextures : List String -> Cmd Msg
loadTextures urls =
    Task.attempt
        (\result ->
            case result of
                Ok textures ->
                    TexturesLoaded textures

                Err _ ->
                    Error "Loading of texture(s) failed"
        )
    <|
        Task.sequence (List.map WebGL.loadTexture urls)


updateFps : Time -> Float -> Float
updateFps t fps =
    let
        threshold =
            fps * 0.1

        newFps =
            1 / inSeconds t
    in
        if (newFps < (fps - threshold)) || (newFps > (fps + threshold)) then
            newFps
        else
            fps


width : Int
width =
    800


height : Int
height =
    600
