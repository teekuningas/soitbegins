module World.Update exposing (updateWorld)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import World.LinearAlgebra as LinearAlgebra
import World.Quaternion as Quaternion exposing (Quaternion(..))
import World.Types exposing (Earth, World)


interpolate : Float -> Float -> Float -> Float -> Float
interpolate from current to stepFraction =
    let
        direction =
            if to - current /= 0 then
                (to - current) / abs (to - current)

            else
                0

        distance =
            abs (to - from)

        step =
            stepFraction * direction * distance
    in
    current + step


updateWorld : Int -> Float -> Float -> Earth -> Earth -> World -> World
updateWorld serverUpdateInterval elapsed previousElapsed msgEarth previousEarthAtMsg world =
    let
        hero =
            world.hero

        earth =
            world.earth

        controller =
            world.controller

        timeInBetween =
            elapsed - previousElapsed

        stepFraction =
            timeInBetween / toFloat serverUpdateInterval

        newRotationAroundAxis =
            interpolate
                previousEarthAtMsg.rotationAroundAxis
                earth.rotationAroundAxis
                msgEarth.rotationAroundAxis
                stepFraction

        newRotationAroundSun =
            interpolate
                previousEarthAtMsg.rotationAroundSun
                earth.rotationAroundSun
                msgEarth.rotationAroundSun
                stepFraction

        newEarth =
            { rotationAroundAxis = newRotationAroundAxis
            , rotationAroundSun = newRotationAroundSun
            }

        newPowerChange =
            if controller.upButtonDown then
                0.0005

            else if controller.downButtonDown then
                -0.0005

            else
                0

        newPower =
            max 0.5
                (min 1.5
                    (hero.power
                        + (timeInBetween * newPowerChange)
                    )
                )

        newAltitudeChange =
            (newPower - 1) / 500

        newAltitude =
            max 105
                (min 150
                    (hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        -- Compute new location
        tiltQuat =
            Quaternion.vecToVec
                (vec3 0 1 0)
                hero.location

        aroundQuat =
            Quaternion.vecToVec
                (Quaternion.transform tiltQuat (vec3 0 0 1))
                hero.orientation

        orientationQuat =
            Quaternion.product aroundQuat tiltQuat

        heroLoc =
            vec3 0 0 0
                |> Quaternion.transform orientationQuat
                |> Vec3.add hero.location

        targetLoc =
            hero.moveDirection
                |> Vec3.scale (1 / 1000)
                |> Quaternion.transform orientationQuat
                |> Vec3.add hero.location

        distance =
            Vec3.scale (timeInBetween * hero.moveSpeed) (Vec3.sub targetLoc heroLoc)

        newLocation =
            Vec3.normalize (Vec3.add heroLoc distance)

        -- Compute new orientation
        ( span1, span2 ) =
            LinearAlgebra.findOrthogonalSpan newLocation

        newOrientation =
            -- Start with previous orientation
            hero.orientation
                |> -- Make exactly same rotation as with the location
                   Quaternion.transform (Quaternion.vecToVec hero.location newLocation)
                |> -- To compensate for numerical inaccuracies,
                   -- project to plane perpendicular to new location
                   -- vector
                   LinearAlgebra.projOntoPlane span1 span2
                |> Vec3.normalize

        -- Wiggling parameter
        newRotationTheta =
            sin (elapsed / 1000) / 20

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , location = newLocation
                , orientation = newOrientation
                , power = newPower
                , altitude = newAltitude
            }
    in
    { world | hero = newHero, earth = newEarth }
