module Main exposing (main)

import AnimationFrame exposing (diffs)
import Walker exposing (Walker)
import Keyboard exposing (KeyCode, downs, ups)
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (vec3, getX, getZ)
import Maze exposing (Maze)
import Html exposing (Html, div, h3, input, label, p, span, text)
import Html.Attributes as Attr
import Html.Events as Evts
import Task exposing (attempt, sequence)
import Time exposing (Time, inSeconds)
import WebGL exposing (Texture)


type alias Model =
    { projection : Mat4
    , walker : Walker
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
    | ToggleAmbientLightning
    | ToggleDiffuseLightning


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
      , walker = Walker.init (vec3 0 1.3 6) 0
      , maze = Nothing
      , fps = 0
      , errStr = Nothing
      }
    , loadTextures
        [ "textures/maze-floor.png"
        , "textures/maze-wall.jpg"
        , "textures/maze-ceiling.jpg"
        , "textures/room-floor.jpg"
        , "textures/outdoor-wall.jpg"
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
            , viewToolbar model
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
                    ++ toString (ceiling (getX model.walker.position))
                    ++ ", Y = "
                    ++ toString (ceiling (getZ model.walker.position))
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
                        (Walker.matrix model.walker)
                        (Walker.position model.walker)
                        (Walker.lightColor model.walker)
                        theMaze

                Nothing ->
                    []
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    case model.maze of
        Just mz ->
            div [ Attr.class "w3-container w3-indigo w3-left-align" ]
                [ checkBox ToggleAmbientLightning
                    "Ambient Lightning"
                  <|
                    Maze.ambientLightning mz
                , checkBox ToggleDiffuseLightning
                    "Diffuse lightning"
                  <|
                    Maze.diffuseLightning mz
                ]

        Nothing ->
            div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            ( { model
                | walker = Walker.animate t model.walker
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
            ( { model | walker = Walker.keyDown code model.walker }
            , Cmd.none
            )

        KeyUp code ->
            ( { model | walker = Walker.keyUp code model.walker }
            , Cmd.none
            )

        TexturesLoaded [ mazeFloorTexture, mazeWallTexture, mazeCeilingTexture, roomFloorTexture, outdoorWallTexture ] ->
            ( { model
                | maze =
                    Just
                        (Maze.init mazeFloorTexture
                            mazeWallTexture
                            mazeCeilingTexture
                            roomFloorTexture
                            outdoorWallTexture
                        )
              }
            , Cmd.none
            )

        TexturesLoaded _ ->
            ( { model | errStr = Just "Unexpected number of textures loaded" }
            , Cmd.none
            )

        ToggleAmbientLightning ->
            ( { model
                | maze =
                    case model.maze of
                        Just mz ->
                            Just (Maze.setAmbientLightning (not <| Maze.ambientLightning mz) mz)

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        ToggleDiffuseLightning ->
            ( { model
                | maze =
                    case model.maze of
                        Just mz ->
                            Just (Maze.setDiffuseLightning (not <| Maze.diffuseLightning mz) mz)

                        Nothing ->
                            Nothing
              }
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


checkBox : Msg -> String -> Bool -> Html Msg
checkBox msg str sel =
    label [ Attr.style [ ( "padding", "20px" ) ] ]
        [ input [ Attr.type_ "checkbox", Attr.checked sel, Evts.onClick msg ]
            []
        , text str
        ]
