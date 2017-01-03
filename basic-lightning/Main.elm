module Main exposing (main)

import AnimationFrame exposing (diffs)
import Lamp exposing (Lamp, make, render, animate, color, position)
import Object exposing (Object, make, render)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makePerspective, makeLookAt)
import Html exposing (Html, button, div, h1, h3, p, select, text, option)
import Html.Attributes as Attr
import Html.Events as Evts
import WebGL exposing (..)
import Time exposing (Time, inSeconds)


type alias Model =
    { projection : Mat4
    , eyePosition : Vec3
    , eyeFocus : Vec3
    , ambientStrength : Float
    , lamp : Lamp
    , object : Object
    , fps : Int
    , rotateLamp : Bool
    , selectedLampColor : String
    }


type Msg
    = Animate Time
    | StartLampRotation
    | StopLampRotation
    | ChangeLampColor String


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
    ( { projection = makePerspective 45 (toFloat width / toFloat height) 0.01 100
      , eyePosition = vec3 1.3 1.4 5
      , eyeFocus = vec3 0 0 0
      , ambientStrength = 0.25
      , lamp = Lamp.make (lampColorMapping "White") 0.1 2.2
      , object = Object.make coral (vec3 0 0 0) 0
      , fps = 0
      , rotateLamp = False
      , selectedLampColor = "White"
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , view3DScene model
        , viewControls model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Attr.class "w3-container w3-blue" ]
        [ h1 []
            [ text <| "Elm WebGL lightning (" ++ toString model.fps ++ " fps)" ]
        ]


view3DScene : Model -> Html Msg
view3DScene model =
    let
        view =
            camera model

        lamp =
            model.lamp
    in
        div [ Attr.class "w3-container w3-black" ]
            [ WebGL.toHtml [ Attr.width width, Attr.height height ]
                [ Lamp.render model.projection view model.lamp
                , Object.render model.projection
                    view
                    (position lamp)
                    (color lamp)
                    model.eyePosition
                    model.ambientStrength
                    model.object
                ]
            ]


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ div [ Attr.class "w3-container w3-blue" ]
            [ h3 []
                [ text "Lightning controls" ]
            ]
        , div [ Attr.class "w3-panel w3-padding-16 w3-light-grey" ]
            [ lampRotationControl model
            , p [] []
            , lampColorControl model
            ]
        ]


lampRotationControl : Model -> Html Msg
lampRotationControl model =
    case model.rotateLamp of
        True ->
            button
                [ Attr.class "w3-btn w3-red"
                , Evts.onClick StopLampRotation
                ]
                [ text "Stop lamp rotation"
                ]

        False ->
            button
                [ Attr.class "w3-btn w3-green"
                , Evts.onClick StartLampRotation
                ]
                [ text "Start lamp rotation"
                ]


lampColorControl : Model -> Html Msg
lampColorControl model =
    select [ Evts.onInput ChangeLampColor ] <|
        List.map
            (\( s, _ ) ->
                option
                    [ Attr.value s
                    , Attr.selected (s == model.selectedLampColor)
                    ]
                    [ text s ]
            )
            availableLampColors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate t ->
            let
                -- Ensure divide by zero not happen.
                deltaS =
                    0.00001 + inSeconds t
            in
                ( { model
                    | lamp =
                        if model.rotateLamp then
                            animate t model.lamp
                        else
                            model.lamp
                    , fps = floor <| 1.0 / deltaS
                  }
                , Cmd.none
                )

        StartLampRotation ->
            ( { model | rotateLamp = True }, Cmd.none )

        StopLampRotation ->
            ( { model | rotateLamp = False }, Cmd.none )

        ChangeLampColor color ->
            ( { model
                | lamp = Lamp.setColor (lampColorMapping color) model.lamp
                , selectedLampColor = color
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Animate


availableLampColors : List ( String, Vec3 )
availableLampColors =
    [ ( "Blue", blue )
    , ( "Green", green )
    , ( "Light Yellow", lightYellow )
    , ( "Red", red )
    , ( "White", white )
    ]


lampColorMapping : String -> Vec3
lampColorMapping selected =
    case List.filter (\( s, _ ) -> s == selected) availableLampColors of
        ( _, color ) :: _ ->
            color

        [] ->
            white


lightYellow : Vec3
lightYellow =
    vec3 1 1 0.5


red : Vec3
red =
    vec3 1 0 0


blue : Vec3
blue =
    vec3 0 0 1


green : Vec3
green =
    vec3 0 1 0


white : Vec3
white =
    vec3 1 1 1


coral : Vec3
coral =
    vec3 1 0.5 0.31


width : Int
width =
    800


height : Int
height =
    600


camera : Model -> Mat4
camera model =
    makeLookAt model.eyePosition model.eyeFocus <| vec3 0 1 0
