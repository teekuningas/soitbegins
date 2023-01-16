module World.Update exposing (updateWorld)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import World.Types exposing (Earth, World)
import World.Quaternion as Quaternion exposing (Quaternion(..))
import World.LinearAlgebra as LinearAlgebra


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
            max 0
                (min 2
                    (hero.power
                        + (timeInBetween * newPowerChange)
                    )
                )

        newAltitudeChange =
            (newPower - 1) / 1000

        newAltitude =
            max 101
                (min 110
                    (hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        -- Compute movement

        tiltQuat =
            (Quaternion.vecToVec
             (vec3 0 1 0)
             (hero.location)
            )

        aroundQuat =
             (Quaternion.vecToVec
              (Quaternion.transform tiltQuat (vec3 0 0 1))
              (hero.orientation)
             )

        orientationQuat = Quaternion.product aroundQuat tiltQuat

        heroLoc =
            (vec3 0 0 0) |>
            Quaternion.transform orientationQuat |>
            Vec3.add hero.location

        targetLoc =
            hero.moveDirection |>
            Quaternion.transform orientationQuat |>
            Vec3.add hero.location

        distance = 
            Vec3.scale (timeInBetween * hero.moveSpeed) (Vec3.sub targetLoc heroLoc)

        newLocation =
            Vec3.normalize (Vec3.add heroLoc distance)

        (span1, span2) = 
            LinearAlgebra.findOrthogonalSpan newLocation

        newOrientation =
            hero.orientation |>
            -- Vec3.add hero.location |>
            Quaternion.transform (Quaternion.vecToVec hero.location newLocation) |>
            -- (\x -> Vec3.sub x newLocation) |>
            Vec3.normalize |>
            LinearAlgebra.projOntoPlane span1 span2 
            
        test2 = Debug.log ("newLocation: " ++ (Debug.toString newLocation)) 0
        test4 = Debug.log ("newOrientation: " ++ (Debug.toString newOrientation)) 0

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

