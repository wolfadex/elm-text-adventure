module Main exposing (main)

import Game exposing (Game)


main : Program () Game Game.Msg
main =
    let
        game1 =
            Game.makeGame "Spaceship"

        ( cockpit, game2 ) =
            Game.addRoom
                "Cockpit"
                "The cockpit of the ship. It has 3 seats and lots of complicated flight controls."
                game1

        ( commonRoom, game3 ) =
            Game.addRoom
                "Common Room"
                "A shared space for the crew. There's some cabinets, a 3D printer, and places to make food."
                game2

        ( sleepingQuarters, game4 ) =
            Game.addRoom
                "Sleeping Quarters"
                "A few beds and a toilet."
                game3

        ( engineRoom, game5 ) =
            Game.addRoom
                "Engine Room"
                "The room is humming with the sounds of the engine. There are storage tanks for fuel and oxygen."
                game4

        ( bloodyKnife, game6 ) =
            Game.createTool
                "Bloody Knife"
                "An acient blade covered in blood. The blood is still warm."
                (\item g ->
                    ( g
                        |> Game.deleteItem item
                        |> Game.endGame
                            "You see nothing. You feel nothing. You smell nothing. Your mind starts to crumble under the nothingness."
                    , """You wave the knife through the air. It seems to cut through space, opening a passage to another plane.

    A long, spindly pale arm reaches through and grabs you, pulling you into the nothingness . . ."""
                    )
                )
                game5

        ( fork, game7 ) =
            Game.createTool
                "Fork"
                "Your standard fork."
                (\_ g -> ( g, "You wave the fork in the air, like you just don't care." ))
                game6
    in
    game7
        |> Game.addConnection
            { from = cockpit
            , to = commonRoom
            , name = "Ladder Down"
            , description = "Ladder to Common Room"
            , locked = False
            , message = "You climb down the ladder to the common room."
            }
        |> Game.addConnection
            { from = commonRoom
            , to = sleepingQuarters
            , name = "Ladder Down"
            , description = "Ladder to Sleeping Quarters"
            , locked = True
            , message = "You climb down the ladder to the sleeping quarters." 
            }
        |> Game.addConnection
            { from = commonRoom
            , to = cockpit
            , name = "Ladder Up"
            , description = "Ladder to Cockpit"
            , locked = False
            , message = "You climb up the ladder to the cockpit."
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = engineRoom
            , name = "Ladder Down"
            , description = "Ladder to Engine Room"
            , locked = False
            , message = "You climb down the ladder to the engine room."
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Ladder Up"
            , description = "Ladder to Common Room"
            , locked = False
            , message = "You climb up the ladder to the common room."
            }
        |> Game.addConnection
            { from = engineRoom
            , to = sleepingQuarters
            , name = "Ladder Up"
            , description = "Ladder to Sleeping Quarters"
            , locked = False
            , message = "You climb up the ladder to the sleeping quarters."
            }
        |> Game.addItemToRoom bloodyKnife cockpit
        |> Game.addItemToRoom fork commonRoom
        |> Game.finalize
            commonRoom
            """You wake up in a blurry haze. You seem to be on a spaceship but you have no idea how you got there.

Last thing you remember, you were drinking with your friends in a British pub."""
        |> Game.program
