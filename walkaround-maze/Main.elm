module Main exposing (main)

import Camera as Camera
import Maze as Maze
import Html exposing (Html, div, h3, p, text)
import Html.Attributes as Attr
import WebGL as WebGL


type alias Model =
    Int


type Msg
    = NoOp


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
    ( 1, Cmd.none )


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
        [ WebGL.toHtml [ Attr.width width, Attr.height height ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


width : Int
width =
    800


height : Int
height =
    600
