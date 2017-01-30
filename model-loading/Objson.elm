module Objson exposing (Triangle, Vertex, decode)

import Json.Decode as Dec exposing (Decoder)
import Math.Vector3 exposing (Vec3, vec3)


type alias Triangle =
    { vertex1 : Vertex
    , vertex2 : Vertex
    , vertex3 : Vertex
    }


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


decode : Decoder (List Triangle)
decode =
    Dec.list decodeTriangle


decodeTriangle : Decoder Triangle
decodeTriangle =
    Dec.map3 Triangle
        (Dec.field "vertex1" decodeVertex)
        (Dec.field "vertex2" decodeVertex)
        (Dec.field "vertex3" decodeVertex)


decodeVertex : Decoder Vertex
decodeVertex =
    Dec.map2 Vertex
        (Dec.field "position" decodeVec3)
        (Dec.field "normal" decodeVec3)


decodeVec3 : Decoder Vec3
decodeVec3 =
    Dec.map3 vec3
        (Dec.field "x" Dec.float)
        (Dec.field "y" Dec.float)
        (Dec.field "z" Dec.float)
