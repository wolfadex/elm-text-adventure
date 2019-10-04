module Game exposing
    ( Game
    , Item(..)
    , Msg(..)
    , View(..)
    , addConnection
    , addItemToRoom
    , addRoom
    , createContainer
    , createTool
    , finalize
    , getCurrentRoom
    , makeGame
    , thingName
    , update
    , setRoom
    , deleteItem
    )

import Dict exposing (Dict)
import Set exposing (Set)


type alias Room =
    { description : String
    , name : String
    , contents : Set Id
    , connections : List Connection
    }


type Item
    = Tool ToolData
    | Container ContainerData


--type alias Being =
--    { description : String
--    , name : String
--    }


type alias ToolData =
    { description : String
    , name : String
    , use : ItemUse
    }


type alias ContainerData =
    { description : String
    , name : String
    , contents : Set Id
    }


type alias ItemUse =
    ItemId -> Game -> ( Game, Message )


type RoomId
    = RoomId Id


type ItemId
    = ItemId Id


type alias Id =
    Int


type alias Name =
    String


type alias Description =
    String


type alias Message =
    String


type alias Game =
    { rooms : Dict Id Room
    , items : Dict Id Item
    , name : String
    , buildId : Int
    , currentRoom : RoomId
    , viewing : View
    , log : List String
    , inventory : Set Id
    }


type View
    = RoomDescription
    | RoomExits
    | RoomInventory
    | PersonInventory


type alias Connection =
    { to : RoomId
    , locked : Locked
    , name : String
    , description : String
    , message : String
    }


type Locked
    = Locked
    | Unlocked


makeGame : Name -> Game
makeGame name =
    { rooms = Dict.empty
    , items = Dict.empty
    , name = name
    , buildId = 0
    , currentRoom = RoomId -1
    , viewing = RoomDescription
    , log = []
    , inventory = Set.empty
    }


addRoom : Name -> Description -> Game -> ( RoomId, Game )
addRoom name description ({ buildId, rooms } as game) =
    ( RoomId buildId
    , { game
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


getCurrentRoom : Game -> Room
getCurrentRoom { rooms, currentRoom } =
    case currentRoom of
        RoomId id ->
            Dict.get id rooms |> Maybe.withDefault fallbackRoom


fallbackRoom : Room
fallbackRoom =
    { name = "The Void"
    , description = "You see nothing. You feel nothing. You smell nothing. Your mind starts to crumble under the nothingness."
    , contents = Set.empty
    , connections = []
    }


thingName : { a | name : String } -> String
thingName { name } =
    name


addConnection : { from : RoomId, to : RoomId, name : String, description : String, message : String } -> Game -> Game
addConnection { from, to, name, description, message } ({ rooms } as game) =
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


finalize : RoomId -> Message -> Game -> Game
finalize initialRoom initialMessage game =
    { game
        | currentRoom = initialRoom
        , log = [ initialMessage ]
    }


type Msg
    = SetView View
    | MoveRoom RoomId Message
    | PickUpItem Id
    | DropItem Id
    | UseItem Id


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        SetView v ->
            ( { game | viewing = v }, Cmd.none )

        MoveRoom nextRoom message ->
            ( game
                |> setRoom nextRoom
                |> addLog message
            , Cmd.none
            )

        PickUpItem item ->
            let
                (RoomId roomId) =
                    game.currentRoom

                itemName =
                    Dict.get item game.items
                        |> getItemName
            in
            ( { game 
                | rooms =
                    Dict.update
                        roomId
                        (Maybe.map
                            (\room ->
                                { room | contents = Set.remove item room.contents }
                            )
                        )
                        game.rooms
                , inventory = Set.insert item game.inventory
              }
                |> addLog ("Picked up " ++ itemName)
            , Cmd.none
            )

        DropItem item ->
            let
                (RoomId roomId) =
                    game.currentRoom

                itemName =
                    Dict.get item game.items
                        |> getItemName


                roomName =
                    Dict.get roomId game.rooms
                        |> Maybe.withDefault fallbackRoom
                        |> .name
            in
            ( { game 
                | rooms =
                    Dict.update
                        roomId
                        (Maybe.map
                            (\room ->
                                { room | contents = Set.insert item room.contents }
                            )
                        )
                        game.rooms
                , inventory = Set.remove item game.inventory
              }
                |> addLog ("Dropped " ++ itemName ++ " in " ++ roomName)
            , Cmd.none
            )

        UseItem item ->
            let
                toolUse =
                    game.items
                        |> Dict.get item
                        |> (\i ->
                                case i of
                                    Just (Tool { use }) ->
                                        use

                                    _ ->
                                        (\_ g -> ( g, "Nothing happens." ))
                            )

                ( nextGame, message ) =
                    toolUse (ItemId item) game
            in
            ( addLog message nextGame, Cmd.none )


getItemName : Maybe Item -> String
getItemName item =
    case item of
        Just (Tool { name }) ->
            name

        Just (Container { name }) ->
            name

        Nothing ->
            "Nothing"


addLog : String -> { a | log : List String } -> { a | log : List String }
addLog message ({ log } as data) =
    { data | log = message :: List.take 10 log }


createTool : Name -> Description -> ItemUse -> Game -> ( ItemId, Game )
createTool name description use ({ buildId, items } as game) =
    let
        item =
            Tool
                { name = name
                , description = description
                , use = use
                }
    in
    ( ItemId buildId
    , { game
        | items =
            Dict.insert buildId item items
        , buildId = buildId + 1
      }
    )


createContainer : Name -> Description -> Game -> ( ItemId, Game )
createContainer name description ({ buildId, items } as game) =
    let
        item =
            Container { name = name, description = description, contents = Set.empty }
    in
    ( ItemId buildId
    , { game
        | items =
            Dict.insert buildId item items
        , buildId = buildId + 1
      }
    )


addItemToRoom : RoomId -> ( ItemId, Game ) -> Game
addItemToRoom (RoomId roomId) ( (ItemId itemId), ({ rooms } as game) ) =
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


setRoom : RoomId -> Game -> Game
setRoom room game =
    { game | viewing = RoomDescription, currentRoom = room }


deleteItem : ItemId -> Game -> Game
deleteItem (ItemId id) game =
    { game
        | items = Dict.remove id game.items
        , inventory = Set.remove id game.inventory
        , rooms =
            Dict.map
                (\_ data -> { data | contents = Set.remove id data.contents })
                game.rooms
    }
