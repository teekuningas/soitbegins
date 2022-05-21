module World.Update exposing (updateGameData)

import Model.Model exposing (Earth, GameData)


updateGameData : Float -> Float -> Float -> Float -> Earth -> Earth -> GameData -> GameData
updateGameData elapsed previousElapsed msgElapsed previousMsgElapsed msgEarth previousMsgEarth gameData =
    let
        timeInBetween =
            elapsed - previousElapsed

        weight =
            (elapsed - previousMsgElapsed)
                / (msgElapsed - previousMsgElapsed)

        weightedAve p1 p2 w =
            p1 + w * (p2 - p1)

        newEarth =
            { rotationAroundAxis = weightedAve previousMsgEarth.rotationAroundAxis msgEarth.rotationAroundAxis weight
            , rotationAroundSun = weightedAve previousMsgEarth.rotationAroundSun msgEarth.rotationAroundSun weight
            }

        newPowerChange =
            if gameData.controller.upButtonDown then
                0.0001

            else if gameData.controller.downButtonDown then
                -0.0001

            else
                0

        newPower =
            max 0
                (min 2
                    (gameData.hero.power
                        + (timeInBetween * newPowerChange)
                    )
                )

        newAltitudeChange =
            (newPower - 1) / 20

        newAltitude =
            max 105
                (min 500
                    (gameData.hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        newLongitude =
            gameData.hero.longitude - timeInBetween * gameData.hero.lonSpeed

        newLatitude =
            gameData.hero.longitude - timeInBetween * gameData.hero.latSpeed


        newRotationTheta =
            sin (elapsed / 1000) / 20

        hero =
            gameData.hero

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , longitude = newLongitude
                , latitude = newLatitude
                , power = newPower
                , altitude = newAltitude
            }

        newGameData =
            { gameData
                | hero = newHero
                , earth = newEarth
            }
    in
    newGameData
