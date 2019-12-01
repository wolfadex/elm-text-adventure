module Game.Internal exposing
    ( BuildData
    , Connection
    , Description
    , Detail(..)
    , FinishData
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
    , RunData
    , Theme(..)
    , addLog
    , fallbackRoom
    , finishGame
    , getCurrentRoom
    , setRoom
    , startGame
    , update
    )

---- TYPES ----

import Dict exposing (Dict)
import Set exposing (Set)


type alias Message =
    String


type alias Id =
    Int


type RoomId
    = RoomId Id


type alias Room =
    { description : String
    , name : String
    , contents : Set Id
    , connections : List Connection
    }


type ItemId
    = ItemId Id


type Item
    = Tool ToolData
    | Container ContainerData


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



--type alias Game =
--    { rooms : Dict Id Room
--    , items : Dict Id Item
--    , name : String
--    , buildId : Int
--    , currentRoom : RoomId
--    , log : List String
--    , inventory : Set Id
--    , mode : Mode
--    , descriptionDetail : Detail
--    , exitsDetail : Detail
--    , roomItemsDetail : Detail
--    , inventoryDetail : Detail
--    , theme : Theme
--    , initialGame : Game
--    }


type Game
    = Building BuildData
    | Running RunData
    | Finished FinishData


type alias BuildData =
    { rooms : Dict Id Room
    , items : Dict Id Item
    , name : String
    , buildId : Int
    , currentRoom : RoomId
    , log : List String
    , inventory : Set Id
    , theme : Theme
    }


type alias RunData =
    { rooms : Dict Id Room
    , items : Dict Id Item
    , name : String
    , buildId : Int
    , currentRoom : RoomId
    , log : List String
    , inventory : Set Id
    , descriptionDetail : Detail
    , exitsDetail : Detail
    , roomItemsDetail : Detail
    , inventoryDetail : Detail
    , theme : Theme
    , initialGame : BuildData
    }


type alias FinishData =
    { log : List String
    , theme : Theme
    , initialGame : BuildData
    }


type Theme
    = Light
    | Dark


type Detail
    = Expanded
    | Collapsed


toggleDetail : Detail -> Detail
toggleDetail detail =
    case detail of
        Expanded ->
            Collapsed

        Collapsed ->
            Expanded


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


type Msg
    = MoveRoom RoomId Message
    | PickUpItem Id
    | DropItem Id
    | UseItem Id
    | Restart
    | ToggleDescription
    | ToggleExits
    | ToggleRoomItems
    | ToggleInventory
    | ToggleTheme


type alias Name =
    String


type alias Description =
    String



---- FUNCTIONS ----


startGame : BuildData -> RunData
startGame data =
    { rooms = data.rooms
    , items = data.items
    , name = data.name
    , buildId = data.buildId
    , currentRoom = data.currentRoom
    , log = data.log
    , inventory = data.inventory
    , theme = data.theme
    , descriptionDetail = Collapsed
    , exitsDetail = Collapsed
    , roomItemsDetail = Collapsed
    , inventoryDetail = Collapsed
    , initialGame = data
    }


finishGame : RunData -> FinishData
finishGame data =
    { log = data.log
    , theme = data.theme
    , initialGame = data.initialGame
    }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case game of
        Building data ->
            case msg of
                MoveRoom nextRoom message ->
                    ( data
                        |> setRoom nextRoom
                        |> addLog message
                        |> Building
                    , Cmd.none
                    )

                ToggleTheme ->
                    ( Building (toggleTheme data)
                    , Cmd.none
                    )

                _ ->
                    ( game, Cmd.none )

        Running data ->
            case msg of
                Restart ->
                    ( Running (startGame data.initialGame), Cmd.none )

                MoveRoom nextRoom message ->
                    ( data
                        |> setRoom nextRoom
                        |> addLog message
                        |> Running
                    , Cmd.none
                    )

                ToggleTheme ->
                    ( Running (toggleTheme data)
                    , Cmd.none
                    )

                PickUpItem item ->
                    let
                        (RoomId roomId) =
                            data.currentRoom

                        itemName =
                            Dict.get item data.items
                                |> getItemName
                    in
                    ( { data
                        | rooms =
                            Dict.update
                                roomId
                                (Maybe.map
                                    (\room ->
                                        { room | contents = Set.remove item room.contents }
                                    )
                                )
                                data.rooms
                        , inventory = Set.insert item data.inventory
                      }
                        |> addLog ("Picked up " ++ itemName)
                        |> Running
                    , Cmd.none
                    )

                DropItem item ->
                    let
                        (RoomId roomId) =
                            data.currentRoom

                        itemName =
                            Dict.get item data.items
                                |> getItemName

                        roomName =
                            Dict.get roomId data.rooms
                                |> Maybe.withDefault fallbackRoom
                                |> .name
                    in
                    ( { data
                        | rooms =
                            Dict.update
                                roomId
                                (Maybe.map
                                    (\room ->
                                        { room | contents = Set.insert item room.contents }
                                    )
                                )
                                data.rooms
                        , inventory = Set.remove item data.inventory
                      }
                        |> addLog ("Dropped " ++ itemName ++ " in " ++ roomName)
                        |> Running
                    , Cmd.none
                    )

                UseItem item ->
                    let
                        toolUse =
                            data.items
                                |> Dict.get item
                                |> (\i ->
                                        case i of
                                            Just (Tool { use }) ->
                                                use

                                            _ ->
                                                \_ g -> ( g, "Nothing happens." )
                                   )

                        ( nextGame, message ) =
                            toolUse (ItemId item) game
                    in
                    case nextGame of
                        Running d ->
                            ( Running (addLog message d), Cmd.none )

                        Finished d ->
                            ( Finished (addLog message d), Cmd.none)

                        Building _ ->
                            ( game, Cmd.none )

                ToggleDescription ->
                    ( Running { data | descriptionDetail = toggleDetail data.descriptionDetail }, Cmd.none )

                ToggleExits ->
                    ( Running { data | exitsDetail = toggleDetail data.exitsDetail }, Cmd.none )

                ToggleRoomItems ->
                    ( Running { data | roomItemsDetail = toggleDetail data.roomItemsDetail }, Cmd.none )

                ToggleInventory ->
                    ( Running { data | inventoryDetail = toggleDetail data.inventoryDetail }, Cmd.none )

        Finished data ->
            case msg of
                Restart ->
                    ( Running (startGame data.initialGame), Cmd.none )

                ToggleTheme ->
                    ( Finished (toggleTheme data)
                    , Cmd.none
                    )

                _ ->
                    ( game, Cmd.none )


toggleTheme : { a | theme : Theme } -> { a | theme : Theme }
toggleTheme data =
    { data
        | theme =
            case data.theme of
                Light ->
                    Dark

                Dark ->
                    Light
    }


addLog : String -> { a | log : List String } -> { a | log : List String }
addLog message ({ log } as data) =
    { data | log = message :: List.take 500 log }


getItemName : Maybe Item -> String
getItemName item =
    case item of
        Just (Tool { name }) ->
            name

        Just (Container { name }) ->
            name

        Nothing ->
            "Nothing"


fallbackRoom : Room
fallbackRoom =
    { name = "The Void"
    , description = "You see nothing. You feel nothing. You smell nothing. Your mind starts to crumble under the nothingness."
    , contents = Set.empty
    , connections = []
    }


getCurrentRoom : { a | rooms : Dict Id Room, currentRoom : RoomId } -> Room
getCurrentRoom { rooms, currentRoom } =
    case currentRoom of
        RoomId id ->
            Dict.get id rooms |> Maybe.withDefault fallbackRoom



---- FUNCTIONS EXPORTED ALL THE WAY ----


setRoom : RoomId -> { a | currentRoom : RoomId } -> { a | currentRoom : RoomId }
setRoom room game =
    { game | currentRoom = room }
