port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Element exposing (Device, DeviceClass(..), Orientation(..))
import Game exposing (Game, Size(..))
import Html exposing (Html)
import Task
import Json.Decode exposing (Value)
import Dict


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { game : Game
    , size : Size
    }


type Msg
    = GameMsg Game.Msg
    | GetViewport Viewport
    | WindowResize Int Int



init : Value -> ( Model, Cmd Msg )
init maybeSavedGame =
    let        
        toolUseBuilder1 =
            Dict.empty

        game1 =
            Game.makeGame "Spaceship"

        ( cockpit, game2 ) =
            Game.addRoom
                "Cockpit"
                "The cockpit of the ship. It has 3 seats and lots of complicated flight controls. And now we're going to make it a really long description for testing."
                game1

        ( commonRoom, game3 ) =
            Game.addRoom
                "Common Room"
                "A shared space for the crew. There are some cabinets, a 3D printer, and places to make food."
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

        bloodyKnifeDecoderKey =
            "bloodyKnife"

        bloodyKnifeUse =
            (\item g ->
                ( g
                    |> Game.deleteItem item
                    |> Game.endGame
                        "You see nothing. You feel nothing. You smell nothing. Your mind starts to crumble under the nothingness."
                , """You wave the knife through the air. It seems to cut through space, opening a passage to another plane.

A long, spindly pale arm reaches through and grabs you, pulling you into the nothingness . . ."""
                )
            )

        toolUseBuilder2 =
            Dict.insert bloodyKnifeDecoderKey bloodyKnifeUse toolUseBuilder1

        ( bloodyKnife, game6 ) =
            Game.createTool
                { name = "Bloody Knife"
                , description = "An acient blade covered in blood. The blood is still warm."
                , use = bloodyKnifeUse
                , decoderKey = bloodyKnifeDecoderKey
                }
                game5

        forkDecoderKey =
            "fork"

        forkUse =
            (\_ g -> ( g, "You wave the fork in the air, like you just don't care." ))

        toolUseBuilder3 =
            Dict.insert forkDecoderKey forkUse toolUseBuilder2

        ( fork, game7 ) =
            Game.createTool
                { name = "Fork"
                , description = "Your standard fork."
                , use = forkUse
                , decoderKey = forkDecoderKey
                }
                game6

        finalGame =
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
                    , locked = False
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
    in
    ( { game =
            case Game.decode toolUseBuilder3 maybeSavedGame of
                Ok savedGame ->
                    savedGame

                Err _ ->
                    finalGame
      , size = Large
      }
    , Browser.Dom.getViewport |> Task.perform GetViewport
    )
    

subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


port saveGame : Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg m ->
            let
                ( newGame, gameCmd ) = Game.update m model.game
            in
            ( { model | game = newGame }
            , Cmd.batch
                [ Cmd.map GameMsg gameCmd
                , newGame
                    |> Game.encode
                    |> saveGame
                ]
            )

        GetViewport { viewport } ->
            let
                device = Element.classifyDevice { width = floor viewport.width, height = floor viewport.height }
            in
            ( { model
                | size = sizeFromDevice device
              }
            , Cmd.none
            )

        WindowResize width height ->
            let
                device = Element.classifyDevice { width = width, height = height }
            in
            ( { model
                | size = sizeFromDevice device
              }
            , Cmd.none
            )


sizeFromDevice : Device -> Size
sizeFromDevice { class, orientation } =
    case class of
        Phone ->
            case orientation of
                Portrait -> Small
                Landscape -> Large
        Tablet ->
            case orientation of
                Portrait -> Small
                Landscape -> Large
        Desktop -> Large
        BigDesktop -> Large


view : Model -> Html Msg
view { game, size } =
    Game.view GameMsg size game
