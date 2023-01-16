module World.Update exposing (updateWorld)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

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

        latitudeRotation =
            Mat4.makeRotate -(pi / 2 - hero.latitude) (Vec3.k)

        longitudeRotation =
            Mat4.makeRotate -hero.longitude (Vec3.j)

        latlonRotation =
            List.foldl
                Mat4.mul
                Mat4.identity
                [ latitudeRotation
                , longitudeRotation
                ]

        direction = 
            Vec3.scale (0.00001) hero.moveDirection
 
        targetXYZ = 
            Mat4.transform 
                latlonRotation 
                (Vec3.normalize 
                    (Vec3.add (vec3 0 1 0) direction))

        heroXYZ = 
            Mat4.transform latlonRotation (vec3 0 1 0)

        newXYZ = 
            (Vec3.sub targetXYZ heroXYZ) |>
            (\x -> Vec3.scale (hero.moveSpeed * timeInBetween) x) |>
            Vec3.add heroXYZ |>
            Vec3.normalize

        newLatitude = 
            asin (Vec3.getY newXYZ)

        newLongitude = 
            atan2 (Vec3.getZ newXYZ) ((Vec3.getX newXYZ))

        newRotationTheta =
            sin (elapsed / 1000) / 20

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , longitude = newLongitude
                , latitude = newLatitude
                , power = newPower
                , altitude = newAltitude
            }
    in
    { world | hero = newHero, earth = newEarth }

