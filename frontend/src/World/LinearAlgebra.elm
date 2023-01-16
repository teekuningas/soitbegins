module World.LinearAlgebra exposing (findOrthogonalSpan, projOntoPlane)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


findOrthogonalSpanZ : Vec3 -> (Vec3, Vec3)
findOrthogonalSpanZ vec =
    let
        a1 =
            Vec3.getX vec
        a2 =
            Vec3.getY vec
        a3 =
            Vec3.getZ vec
        b1 =
            a1/a3
        b2 =
            a2/a3
        b3 = 
            1
    in
        (Vec3.normalize (vec3 1 0 -b1), Vec3.normalize (vec3 0 1 -b2))

findOrthogonalSpanY : Vec3 -> (Vec3, Vec3)
findOrthogonalSpanY vec =
    let
        a1 =
            Vec3.getX vec
        a2 = 
            Vec3.getY vec
        a3 =
            Vec3.getZ vec
        b1 =
            a1/a2
        b2 =
            1
        b3 = 
            a3/a2
    in
        (Vec3.normalize (vec3 1 -b1 0), Vec3.normalize (vec3 0 -b3 1))

findOrthogonalSpanX : Vec3 -> (Vec3, Vec3)
findOrthogonalSpanX vec =
    let
        a1 =
            Vec3.getX vec
        a2 =
            Vec3.getY vec
        a3 =
            Vec3.getZ vec
        b1 =
            1
        b2 =
            a2/a1
        b3 = 
            a3/a1
    in
        (Vec3.normalize (vec3 -b2 1 0), Vec3.normalize (vec3 -b3 0 1))


findOrthogonalSpan : Vec3 -> (Vec3, Vec3)
findOrthogonalSpan vec =
    let
        vecNorm = Vec3.normalize vec

        x =
            Vec3.getX vecNorm
        y =
            Vec3.getY vecNorm
        z =
            Vec3.getZ vecNorm
     in 
        if (z > 0.1) then
            findOrthogonalSpanZ vecNorm
        else if (y > 0.1) then
            findOrthogonalSpanY vecNorm
        else
            findOrthogonalSpanX vecNorm


projOntoPlane : Vec3 -> Vec3 -> Vec3 -> Vec3
projOntoPlane u1 u2 y =
    Vec3.add
        (Vec3.scale (Vec3.dot y u1) u1)
        (Vec3.scale (Vec3.dot y u2) u2)

