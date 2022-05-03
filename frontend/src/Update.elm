module Update exposing (updateGameData)

import Common exposing (Earth, GameData)


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
            { rotationTheta = weightedAve previousMsgEarth.rotationTheta msgEarth.rotationTheta weight
            , locationX = weightedAve previousMsgEarth.locationX msgEarth.locationX weight
            , locationY = weightedAve previousMsgEarth.locationY msgEarth.locationY weight
            , locationZ = weightedAve previousMsgEarth.locationZ msgEarth.locationZ weight
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
            (newPower - 1) / 200

        newAltitude =
            max 10.5
                (min 100
                    (gameData.hero.altitude
                        + (timeInBetween * newAltitudeChange)
                    )
                )

        newLongitude =
            gameData.hero.longitude - timeInBetween * 0.00005

        newRotationTheta =
            sin (elapsed / 1000) / 20

        hero =
            gameData.hero

        newHero =
            { hero
                | rotationTheta = newRotationTheta
                , longitude = newLongitude
                , power = newPower
                , altitude = newAltitude
            }

        newGameData =
            { gameData
                | hero = newHero
                , earth = Just newEarth
            }
    in
    newGameData
