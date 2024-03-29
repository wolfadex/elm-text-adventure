module Game exposing
    ( Game
    , Msg
    , update
    , view
    , makeGame
    , finalize
    , endGame
    , addRoom
    , changeRoomName
    , changeRoomDescription
    , getCurrentRoom
    , setRoom
    , deleteRoom
    , addConnection
    , deleteConnection
    , createTool
    , changeItemName
    , changeItemDescription
    , changeItemUse
    , addItemToRoom
    , deleteItem
    ,  Size(..)
       --, createContainer
    , encode
    , decode
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

@docs update
@docs view
@docs makeGame
@docs finalize
@docs endGame
@docs Size
@docs encode
@docs decode


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

import Dict exposing (Dict)
import Game.Internal
    exposing
        ( Description
        , Detail(..)
        , Game(..)
        , Id
        , Item(..)
        , ItemId(..)
        , ItemUse
        , Locked(..)
        , Message
        , Msg(..)
        , Name
        , Room
        , RoomId(..)
        , Theme(..)
        , addLog
        )
import Game.View exposing (ParentMsg, Size(..))
import Html exposing (Html)
import Json.Decode exposing (Value)
import Set


{-| -}
type Game
    = Game Game.Internal.Game


{-| -}
type alias Msg =
    Game.Internal.Msg


{-| Used for determining the way the game is displayed.

When Large, the log is displayed on the right and interactions on the left.

When Small, the log is displayed on the bottom and interactions on the top. Interactions are also collapsible so they take up less space.
-}
type Size
    = Large
    | Small


{-| Displays the game, taking a parent Msg.
-}
view : ParentMsg msg -> Size -> Game -> Html msg
view parentMsg size (Game game) =
    Game.View.view
        parentMsg
        (case size of
            Large ->
                Game.View.Large

            Small ->
                Game.View.Small
        )
        game


{-| Updates the game state.
-}
update : Msg -> Game -> ( Game, Cmd Msg )
update msg (Game game) =
    Game.Internal.update msg game
        |> Tuple.mapFirst Game


{-| Takes a name and creates your new game.

    makeGame "Your Cool Adventure"

-}
makeGame : Name -> Game
makeGame name =
    { rooms = Dict.empty
    , items = Dict.empty
    , name = name
    , buildId = 0
    , currentRoom = RoomId -1
    , log = []
    , inventory = Set.empty
    , theme = Light
    }
        |> Building
        |> Game


{-| Adds a new room to your game. This only creates the room and not any connections between rooms.

    addRoom "Name" "Description" yourGame

-}
addRoom : Name -> Description -> Game -> ( RoomId, Game )
addRoom name description (Game game) =
    case game of
        Building ({ buildId, rooms } as data) ->
            ( RoomId buildId
            , { data
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
                |> Building
                |> Game
            )

        Running ({ buildId, rooms } as data) ->
            ( RoomId buildId
            , { data
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
                |> Running
                |> Game
            )

        Finished _ ->
            ( RoomId -1, Game game )


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
    case game of
        Building data ->
            updateRoomHelper changeToMake roomId data
                |> Building
                |> Game

        Running data ->
            updateRoomHelper changeToMake roomId data
                |> Running
                |> Game

        Finished _ ->
            Game game


updateRoomHelper : (Room -> Room) -> Id -> { g | rooms : Dict Id Room } -> { g | rooms : Dict Id Room }
updateRoomHelper changeToMake roomId game =
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
    case game of
        Building data ->
            deleteRoomHelper roomId data |> Building |> Game

        Running data ->
            deleteRoomHelper roomId data |> Running |> Game

        Finished _ ->
            Game game


deleteRoomHelper : Id -> { g | rooms : Dict Id Room, items : Dict Id Item } -> { g | rooms : Dict Id Room, items : Dict Id Item }
deleteRoomHelper roomId game =
    case Dict.get roomId game.rooms of
        Nothing ->
            game

        Just { contents } ->
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
getCurrentRoom (Game game) =
    case game of
        Building { currentRoom } ->
            currentRoom

        Running { currentRoom } ->
            currentRoom

        Finished _ ->
            RoomId -1


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
addConnection connection (Game game) =
    case game of
        Building data ->
            addConnectionHelper connection data |> Building |> Game

        Running data ->
            addConnectionHelper connection data |> Running |> Game

        Finished _ ->
            Game game


addConnectionHelper : { from : RoomId, to : RoomId, name : String, description : String, locked : Bool, message : String } -> { g | rooms : Dict Id Room } -> { g | rooms : Dict Id Room }
addConnectionHelper { from, to, name, description, locked, message } ({ rooms } as game) =
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
                                , locked =
                                    if locked then
                                        Locked

                                    else
                                        Unlocked
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
    case game of
        Building data ->
            Game.Internal.startGame
                { data
                    | currentRoom = initialRoom
                    , log = [ initialMessage ]
                }
                |> Running
                |> Game

        _ ->
            Game game


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
createTool : { name : Name, description : Description, use : (ItemId -> Game -> ( Game, Message )), decoderKey : String } -> Game -> ( ItemId, Game )
createTool args (Game game) =
    case game of
        Building data ->
            createToolHelper args data
                |> Tuple.mapSecond Building
                |> Tuple.mapSecond Game

        Running data ->
            createToolHelper args data
                |> Tuple.mapSecond Running
                |> Tuple.mapSecond Game

        Finished _ ->
            ( ItemId -1, Game game )


createToolHelper : { name : Name, description : Description, use : (ItemId -> Game -> ( Game, Message )), decoderKey : String } -> { g | buildId : Id, items : Dict Id Item } -> ( ItemId, { g | buildId : Id, items : Dict Id Item } )
createToolHelper { name, description, use, decoderKey } ({ buildId, items } as game) =
    let
        item =
            Tool
                { name = name
                , description = description
                , use = mapToolUse use
                , decoderKey = decoderKey
                }
    in
    ( ItemId buildId
    , { game
        | items =
            Dict.insert buildId item items
        , buildId = buildId + 1
      }
    )


mapToolUse : (ItemId -> Game -> ( Game, Message )) -> ItemUse
mapToolUse use =
    \i g -> use i (Game g) |> Tuple.mapFirst extractGame


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
    updateItem
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
    case game of
        Building data ->
            { data
                | items =
                    Dict.update
                        itemId
                        (Maybe.map changeToMake)
                        data.items
            }
                |> Building
                |> Game

        Running data ->
            { data
                | items =
                    Dict.update
                        itemId
                        (Maybe.map changeToMake)
                        data.items
            }
                |> Running
                |> Game

        Finished _ ->
            Game game



--createContainer : Name -> Description -> Game -> ( ItemId, Game )
--createContainer name description (Game ({ buildId, items } as game)) =
--    let
--        item =
--            Container { name = name, description = description, contents = Set.empty }
--    in
--    ( ItemId buildId
--    , Game
--        { game
--            | items =
--                Dict.insert buildId item items
--            , buildId = buildId + 1
--        }
--    )


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
    case game of
        Running data ->
            data
                |> Game.Internal.setRoom room
                |> Running
                |> Game

        Building data ->
            data
                |> Game.Internal.setRoom room
                |> Building
                |> Game

        Finished _ ->
            Game game


{-| Removes the specified item from the game.

    deleteItem itemToDelete yourGame

-}
deleteItem : ItemId -> Game -> Game
deleteItem (ItemId id) (Game game) =
    case game of
        Building data ->
            { data
                | items = Dict.remove id data.items
                , inventory = Set.remove id data.inventory
                , rooms =
                    Dict.map
                        (\_ d -> { d | contents = Set.remove id d.contents })
                        data.rooms
            }
                |> Building
                |> Game

        Running data ->
            { data
                | items = Dict.remove id data.items
                , inventory = Set.remove id data.inventory
                , rooms =
                    Dict.map
                        (\_ d -> { d | contents = Set.remove id d.contents })
                        data.rooms
            }
                |> Running
                |> Game

        Finished _ ->
            Game game


{-| Use this to end the game. Pass it your final message.

    endGame "Final message" yourGame

-}
endGame : String -> Game -> Game
endGame endMessage (Game game) =
    case game of
        Running data ->
            Game.Internal.finishGame data
                |> addLog endMessage
                |> Finished
                |> Game

        _ ->
            Game game


{-| Encode the game so that it can be saved to localStorage or elsewhere.
-}
encode : Game -> Value
encode (Game game) =
    Game.Internal.encodeGame game


{-| Decode a game that was saved.

Also requires a Dict String ItemUse of the form

    Dict.fromList
        [ ( "uniqueNameForItem", (\itemId, game -> ... ( modifiedGame, "Message about what happened." )) ) ]

This is used to rebuild the `use` function of the tools you've created.
-}
decode : Dict String (ItemId -> Game -> ( Game, Message )) -> Value -> Result Json.Decode.Error Game
decode toolUseBuilder value =
    case Json.Decode.decodeValue (Game.Internal.decodeGame (Dict.map (\_ use -> mapToolUse use) toolUseBuilder)) value of
        Ok g -> Ok (Game g)
        Err err -> Err err
