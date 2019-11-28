module Game exposing
    ( Game
    , Msg
    , program
    , makeGame
    , changeRoomName
    , changeRoomDescription
    , finalize
    , endGame
    , addRoom
    , deleteRoom
    , getCurrentRoom
    , setRoom
    , addConnection
    , createTool
    , changeItemName
    , changeItemDescription
    , changeItemUse
    , addItemToRoom
    , deleteItem
    , deleteConnection
    --, createContainer
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
        Game.makeGame "Sample Game"
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
@docs changeRoomName
@docs changeRoomDescription
@docs getCurrentRoom
@docs setRoom
@docs deleteRoom
@docs addConnection
@docs deleteConnection


## Items

@docs createTool
@docs changeItemName
@docs changeItemDescription
@docs changeItemUse
@docs addItemToRoom
@docs deleteItem

-}

import Browser
import Dict
import Game.Internal
    exposing
        ( Description
        , Game
        , Item(..)
        , ItemId(..)
        , ItemUse
        , Locked(..)
        , Message
        , Mode(..)
        , Msg(..)
        , Name
        , Room
        , RoomId(..)
        , View(..)
        , addLog
        )
import Game.View
import Set


{-| -}
type Game
    = Game Game.Internal.Game


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


{-| Change the name of a room.

    changeRoomName "New Name" someRoom yourGame
-}
changeRoomName : Name -> RoomId -> Game -> Game
changeRoomName newName =
    updateRoom (\room -> { room | name = newName })


{-| Change the namedescription of a room.

    changeRoomDescription "New Description" someRoom yourGame
-}
changeRoomDescription : Description -> RoomId -> Game -> Game
changeRoomDescription newDescription =
    updateRoom (\room -> { room | description = newDescription })


{-| Helper for updating rooms.
-}
updateRoom : (Room -> Room) -> RoomId -> Game -> Game
updateRoom changeToMake (RoomId roomId) (Game game) =
    Game
        { game
            | rooms =
                Dict.update
                    roomId
                    (Maybe.map changeToMake)
                    game.rooms
        }


{-| Removes a room from the game, as well as all of its contents and any connections going to the room.

    deleteRoom someRoom yourGame

**NOTE:** If you delete the room your character is currently in, don't forget to send them to a new room with `setRoom`!
-}
deleteRoom : RoomId -> Game -> Game
deleteRoom (RoomId roomId) (Game game) =
    case Dict.get roomId game.rooms of
        Nothing ->
            Game game

        Just { contents } ->
            Game
                { game
                    | rooms =
                        game.rooms
                            |> Dict.remove roomId
                            |> Dict.map (\_ r -> { r | connections = List.filter (\{ to } -> to == RoomId roomId) r.connections })
                    , items =
                        Dict.filter
                            (\id _ -> not (Set.member id contents))
                            game.items
                }




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
addConnection : { from : RoomId, to : RoomId, name : String, description : String, locked : Bool, message : String } -> Game -> Game
addConnection { from, to, name, description, locked, message } (Game ({ rooms } as game)) =
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
                                    , locked = if locked then Locked else Unlocked
                                    , message = message
                                    }
                                        :: connections
                            }
                        )
                    )
                    rooms
        }


{-| Remove the specified connection from the game. A connection is identified by the room it's coming from, going to, and its name.

    deleteConnection { from = roomId1, to = roomId2, name = "Name" } yourGame
-}
deleteConnection : { from : RoomId, to : RoomId, name : Name } -> Game -> Game
deleteConnection { from, to, name } =
    updateRoom
        (\room ->
            { room
                | connections =
                    List.filter
                        (\connection -> connection.to /= to || connection.name /= name)
                        room.connections
            }
        )
        from


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


{-| Change the name of an item.

    changeItemName "New Name" item game
-}
changeItemName : Name -> ItemId -> Game -> Game
changeItemName newName =
    updateItem
        (\item ->
            case item of
                Tool t ->
                    Tool { t | name = newName }

                Container c ->
                    Container { c | name = newName }
        )


{-| Change the description of an item.

    changeItemDescription "New description." item game
-}
changeItemDescription : Description -> ItemId -> Game -> Game
changeItemDescription newDescription =
    updateItem
        (\item ->
            case item of
                Tool t ->
                    Tool { t | description = newDescription }

                Container c ->
                    Container { c | description = newDescription }
        )


{-| Change what happens when you use an item.

    changeItemUse (\itemId game -> ...) item game
-}
changeItemUse : ItemUse -> ItemId -> Game -> Game
changeItemUse newUse =
    updatItem
        (\item ->
            case item of
                Container _ ->
                    item

                Tool t ->
                    Tool { t | use = newUse }
        )


{-| Helper for updating items.
-}
updateItem : (Item -> Item) -> ItemId -> Game -> Game
updateItem changeToMake (ItemId itemId) (Game game) =
    Game
        { game
            | items =
                Dict.update
                    itemId
                    (Maybe.map changeToMake)
                    game.items
        }


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

    addItemToRoom item room game
-}
addItemToRoom : ItemId -> RoomId -> Game -> Game
addItemToRoom (ItemId itemId) =
    updateRoom (\room -> { room | contents = Set.insert itemId room.contents })


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
