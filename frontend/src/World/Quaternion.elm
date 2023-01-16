module World.Quaternion exposing (Quaternion(..), product, identity)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


-- Assume unit quaternions everywhere


type Quaternion = 
    Quat Float Float Float Float


identity : Quaternion
identity = 
    Quat 1 0 0 0


product : Quaternion -> Quaternion -> Quaternion
product q1 q2 =
    case (q1, q2) of 
        ((Quat a1 b1 c1 d1), (Quat a2 b2 c2 d2)) -> 
            (Quat 
             (a1*a2 - b1*b2 - c1*c2 - d1*d2)
             (a1*b2 + b1*a2 + c1*d2 - d1*c2)
             (a1*c2 - b1*d2 + c1*a2 + d1*b2)
             (a1*d2 + b1*c2 - c1*b2 + d1*a2)
            )


conjugate : Quaternion -> Quaternion
conjugate q =
    case q of
        Quat a1 b2 c3 d4 ->
            Quat a1 -b2 -c3 -d4


transform : Quaternion -> Vec3 -> Vec3
transform q v =
    let 
        vQuat =
            (Quat 0 (Vec3.getX v) (Vec3.getY v) (Vec3.getZ v))
    in
        getVec (product (product q vQuat) (conjugate q))


rotate : Vec3 -> Float -> Quaternion
rotate axis angle =
    (Quat 
     (cos (angle/2)) 
     ((sin (angle/2))*(Vec3.getX axis))
     ((sin (angle/2))*(Vec3.getY axis))
     ((sin (angle/2))*(Vec3.getZ axis))
    )


getVec : Quaternion -> Vec3
getVec q =
    case q of 
        (Quat w x y z) ->
            (vec3 x y z)


getScalar : Quaternion -> Float
getScalar q =
    case q of 
        (Quat w x y z) ->
            w


vecToVec : Vec3 -> Vec3 -> Quaternion
vecToVec v1 v2 =
    if (Vec3.dot v1 v2) >= 0.9999 then
        identity
    else if (Vec3.dot v1 v2) <= -0.9999 then
        let
            helperVec =
                (vec3 0 0 1)
        in
            rotate (Vec3.cross v1 helperVec) pi 
    else
        let 
            w = 
                1 + (Vec3.dot v1 v2)

            xyz =
                (Vec3.cross v1 v2)

            x =
                Vec3.getX xyz

            y =
                Vec3.getY xyz

            z =
                Vec3.getZ xyz

            length = sqrt (w*w + x*x + y*y + z*z)

        in
            (Quat 
             (w / length)
             (x / length)
             (y / length)
             (z / length)
            )
            
