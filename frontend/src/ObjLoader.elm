module ObjLoader exposing (objMeshDecoder)

import Array

import Length exposing (Meters, inMeters)
import Quantity exposing (Unitless)
import Obj.Decode
import Obj.Decode exposing (ObjCoordinates)
import TriangularMesh exposing (TriangularMesh)
import Point3d exposing (Point3d, toRecord)
import Vector3d exposing (Vector3d)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)

import WebGL exposing (Mesh)

import Common exposing (Vertex)


triangularMeshToMeshVertex :
  (TriangularMesh
    { position : Point3d Meters ObjCoordinates
    , normal : Vector3d Unitless ObjCoordinates
    }
  ) -> Mesh Vertex
triangularMeshToMeshVertex triangularMesh = 
  let vertices = TriangularMesh.faceVertices triangularMesh

      getColor loc = 
        if (Vec3.length loc) <= 1.00 then (vec3 0 0 1)
        else (if (Vec3.length loc) >= 1.03 
              then (vec3 (139/255) (69/255) (19/255))
              else (vec3 (34/255) (139/255) (34/255)))

      posToVertex val = 
        let pos = Vec3.fromRecord (toRecord inMeters val.position)
        in
        { color = getColor pos
        , position = pos
        }

      convTriangle tri = case tri of (x, y, z) -> ( posToVertex x
                                                  , posToVertex y
                                                  , posToVertex z )

      earthMesh = (List.map convTriangle vertices)
  in
    WebGL.triangles earthMesh


objMeshDecoder : Obj.Decode.Decoder (Mesh Vertex)
objMeshDecoder =
  Obj.Decode.map triangularMeshToMeshVertex Obj.Decode.faces
