module Game exposing
    ( Game
    , Msg
    , addConnection
    , addItemToRoom
    , addRoom
    --, createContainer
    , createTool
    , finalize
    , getCurrentRoom
    , makeGame
    , setRoom
    , deleteItem
    , endGame
    , program
    )


{-| The following is a basic game with 2 rooms and connections to move between them.

    import Game

    {-| The most basic game possible
    -}
    main =
        let
            initialGame =
                Game.makeGame "Simple Game"

            ( bedroom, initialGame ) =
                Game.addRoom
                    "Bedroom"
                    "A simple bedroom. Just a bed and 3 drawer dresser."

            ( bathroom, initialGame ) =
                Game.addRoom
                    "Bathroom"
                    "A simple bathroom. Just a toilet and sink."
        in
        Gma.makeGame "Sample Game"
            |> Game.addConnection
                { from = bedroom
                , to = bathroom
                , name = "Bathroom Door"
                , description = "Door to the bathroom."
                , message = "You walk to the bathroom."
                }
            |> Game.addConnection
                { from = bathroom
                , to = bedroom
                , name = "Bedroom Door"
                , description = "Door to the bedroom."
                , message = "You walk to the bedroom."
                }
            |> Game.finalize
                bedroom
                "You wake up in your bedroom."
            |> Game.program

# Types for makeing your `main` function typed

@docs Game
@docs Msg

# Game building functions

## General

@docs program
@docs makeGame
@docs finalize
@docs endGame

## Rooms

@docs addRoom
@docs getCurrentRoom
@docs setRoom
@docs addConnection

## Items

@docs createTool
@docs addItemToRoom
@docs deleteItem

-}

import Browser
import Dict exposing (Dict)
import Set exposing (Set)
import Game.View
import Game.Internal exposing
    ( Msg(..)
    , Item(..)
    , View(..)
    , ItemId(..)
    , RoomId(..)
    , Game
    , Message
    , Locked(..)
    , Room
    , Name
    , Description
    , Mode(..)
    , addLog
    )


{-| -}
type Game =
    Game Game.Internal.Game


{-| -}
type alias Msg =
    Game.Internal.Msg


{-| Starts and displays your game.

    main : Program () Game Msg
    main =
        let
            ...
        in
            Game.program yourGame
-}
program : Game -> Program () Game Msg
program game =
    Browser.element
        { init = \_ -> ( game, Cmd.none )
        , view = \(Game g) -> Game.View.view g
        , update = \msg (Game g) -> Game.Internal.update msg g |> Tuple.mapFirst Game
        , subscriptions = \_ -> Sub.none    
        }


{-| Takes a name and creates your new game.

    makeGame "Your Cool Adventure"
-}
makeGame : Name -> Game
makeGame name =
    Game
        { rooms = Dict.empty
        , items = Dict.empty
        , name = name
        , buildId = 0
        , currentRoom = RoomId -1
        , viewing = RoomDescription
        , log = []
        , inventory = Set.empty
        , mode = Building
        }


{-| Adds a new room to your game. This only creates the room and not any connections between rooms.

    addRoom "Name" "Description" yourGame
-}
addRoom : Name -> Description -> Game -> ( RoomId, Game )
addRoom name description (Game ({ buildId, rooms } as game)) =
    ( RoomId buildId
    , Game
        { game
            | rooms =
                Dict.insert
                    buildId
                    { name = name
                    , description = description
                    , contents = Set.empty
                    , connections = []
                    }
                    rooms
            , buildId = buildId + 1
        }
    )


{-| Gets the current room the player is in. Useful for knowing where the player is when they use an item.

    getCurrentRoom yourGame == someRoom
-}
getCurrentRoom : Game -> RoomId
getCurrentRoom (Game { currentRoom }) =
    currentRoom


{-| Creates a one-way connection between 2 rooms.

In order for to get from room A to room B and back, you need to create 2 connections.

    existingGameWithRooms
        |> Game.addConnection
            { from = roomA
            , to = roomB
            , name = "Door"
            , description = "Door to Room B."
            , message = "You walk to Room B."
            }
        |> Game.addConnection
            { from = roomB
            , to = roomA
            , name = "Door"
            , description = "Door to Room A."
            , message = "You walk to Room A."
            }
-}
addConnection : { from : RoomId, to : RoomId, name : String, description : String, message : String } -> Game -> Game
addConnection { from, to, name, description, message } (Game ({ rooms } as game)) =
    Game
        { game
            | rooms =
                Dict.update
                    (case from of
                        RoomId id ->
                            id
                    )
                    (Maybe.map
                        (\({ connections } as room) ->
                            { room
                                | connections =
                                    { name = name
                                    , description = description
                                    , to = to
                                    , locked = Unlocked
                                    , message = message
                                    }
                                        :: connections
                            }
                        )
                    )
                    rooms
        }


{-| Finalizes your game by setting the initial room and the first message the player sees.

    finalize startingRoom "Welcome message to set the scene" yourGame
-}
finalize : RoomId -> Message -> Game -> Game
finalize initialRoom initialMessage (Game game) =
    Game
        { game
            | currentRoom = initialRoom
            , log = [ initialMessage ]
            , mode = Running
        }


{-| Creates a tool item. A tool is any item that can be used such as a fork, sword, key, or potato.

    createTool
        "Name"
        "Description"
        (\itemId gameState ->
            ( updatedGameState, "Message describing what happened" )
        )
        yourGame

ItemUse is how your item affects the game world, anything from opening a door to teleporting the player.
-}
createTool : Name -> Description -> (ItemId -> Game -> ( Game, Message )) -> Game -> ( ItemId, Game )
createTool name description use (Game ({ buildId, items } as game)) =
    let
        item =
            Tool
                { name = name
                , description = description
                , use = \i g -> use i (Game g) |> Tuple.mapFirst extractGame
                }
    in
    ( ItemId buildId
    , Game
        { game
            | items =
                Dict.insert buildId item items
            , buildId = buildId + 1
        }
    )


extractGame : Game -> Game.Internal.Game
extractGame (Game g) =
    g


{-| Creates a container that can be placed in a room or on your player.

    createContainer "Name" "Description" yourGame
-}
createContainer : Name -> Description -> Game -> ( ItemId, Game )
createContainer name description (Game ({ buildId, items } as game)) =
    let
        item =
            Container { name = name, description = description, contents = Set.empty }
    in
    ( ItemId buildId
    , Game
        { game
            | items =
                Dict.insert buildId item items
            , buildId = buildId + 1
        }
    )


{-| Adds the specified item to the game.

    addItemToRoom room ( item, game )
-}
addItemToRoom : RoomId -> ( ItemId, Game ) -> Game
addItemToRoom (RoomId roomId) ( (ItemId itemId), (Game ({ rooms } as game)) ) =
    Game
        { game
            | rooms =
                Dict.update
                    roomId
                    (Maybe.map
                        (\({ contents } as room) ->
                            { room | contents = Set.insert itemId contents }
                        )
                    )
                    rooms
        }


{-| "Teleports" the player to the specified room.

    setRoom someRoom yourGame
-}
setRoom : RoomId -> Game -> Game
setRoom room (Game game) =
    game
        |> Game.Internal.setRoom room
        |> Game


{-| Removes the specified item from the game.

    deleteItem itemToDelete yourGame
-}
deleteItem : ItemId -> Game -> Game
deleteItem (ItemId id) (Game game) =
    Game
        { game
            | items = Dict.remove id game.items
            , inventory = Set.remove id game.inventory
            , rooms =
                Dict.map
                    (\_ data -> { data | contents = Set.remove id data.contents })
                    game.rooms
        }


{-| Use this to end the game. Pass it your final message.

    endGame "Final message" yourGame
-}
endGame : String -> Game -> Game
endGame endMessage (Game game) =
    { game | mode = Finished }
        |> addLog endMessage
        |> Game

