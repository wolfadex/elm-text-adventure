module Main exposing (main)

import Browser exposing (Document)
import Game exposing (Game, addRoom, makeGame)
import Game.View


main : Program () Game Game.Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Game, Cmd Game.Msg )
init _ =
    let
        game1 =
            makeGame "Spaceship"

        ( cockpit, game2 ) =
            addRoom
                "Cockpit"
                "The cockpit of the ship. It has 3 seats and lots of complicated flight controls."
                game1

        ( commonRoom, game3 ) =
            addRoom
                "Common Room"
                "A shared space for the crew. There's some cabinets, a 3D printer, and places to make food."
                game2

        ( sleepingQuarters, game4 ) =
            addRoom
                "Sleeping Quarters"
                "A few beds and a toilet."
                game3

        ( engineRoom, game5 ) =
            addRoom
                "Engine Room"
                "The room is humming with the sounds of the engine. There are storage tanks for fuel and oxygen."
                game4
    in
    ( game5
        |> Game.addConnection
            { from = cockpit
            , to = commonRoom
            , name = "Ladder Down"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = commonRoom
            , to = cockpit
            , name = "Ladder Up"
            , description = "Ladder to Cockpit"
            }
        |> Game.addConnection
            { from = commonRoom
            , to = sleepingQuarters
            , name = "Ladder Down"
            , description = "Ladder to Sleeping Quarters"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Ladder Up"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Carl"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Carl"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Carl"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = commonRoom
            , name = "Carl"
            , description = "Ladder to Common Room"
            }
        |> Game.addConnection
            { from = sleepingQuarters
            , to = engineRoom
            , name = "Ladder Down"
            , description = "Ladder to Engine Room"
            }
        |> Game.addConnection
            { from = engineRoom
            , to = sleepingQuarters
            , name = "Ladder Up"
            , description = "Ladder to Sleeping Quarters"
            }
        |> Game.init sleepingQuarters
    , Cmd.none
    )


subscriptions : Game -> Sub Game.Msg
subscriptions _ =
    Sub.none


update : Game.Msg -> Game -> ( Game, Cmd Game.Msg )
update =
    Game.update


view : Game -> Document Game.Msg
view model =
    { title = "Text Adventure"
    , body =
        [ Game.View.view model ]
    }
