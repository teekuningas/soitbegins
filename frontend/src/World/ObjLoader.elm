module World.ObjLoader exposing (earthMeshDecoder)

import Length exposing (Meters, inMeters)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Obj.Decode exposing (ObjCoordinates)
import Point3d exposing (Point3d, toRecord)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL exposing (Mesh)
import World.Types exposing (Vertex, MeshList)


triangularEarthToMeshVertex :
    TriangularMesh
        { position : Point3d Meters ObjCoordinates
        , normal : Vector3d Unitless ObjCoordinates
        }
    -> Mesh Vertex
triangularEarthToMeshVertex triangularMesh =
    let
        vertices =
            TriangularMesh.faceVertices triangularMesh

        getColor loc =
            let
                length =
                    Vec3.length loc

                lowlimit =
                    1.0

                highlimit =
                    1.03

                factor =
                    (length - lowlimit) / (highlimit - lowlimit)
            in
            if length <= lowlimit then
                vec3 0 0 1

            else if length >= highlimit then
                vec3 0.54 0.27 0.075

            else
                vec3 (factor * 0.54 + (1 - factor) * 0.5)
                    (factor * 0.27 + (1 - factor) * 1.0)
                    (factor * 0.075 + (1 - factor) * 0.0)

        posToVertex val =
            let
                pos =
                    Vec3.fromRecord (toRecord inMeters val.position)
            in
            { color = getColor pos
            , position = pos
            }

        convTriangle tri =
            case tri of
                ( v1, v2, v3 ) ->
                    ( posToVertex v1
                    , posToVertex v2
                    , posToVertex v3
                    )

        earthMesh =
            List.map convTriangle vertices
    in
    WebGL.triangles earthMesh


earthMeshDecoder : Obj.Decode.Decoder (Mesh Vertex)
earthMeshDecoder =
    Obj.Decode.map triangularEarthToMeshVertex Obj.Decode.faces
