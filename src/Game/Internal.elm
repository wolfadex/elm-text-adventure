module Game.Internal exposing
    ( Description
    , Game
    , Connection
    , Id
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
    , fallbackRoom
    , getCurrentRoom
    , setRoom
    , update
    )

---- TYPES ----

import Browser.Navigation
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


type alias Game =
    { rooms : Dict Id Room
    , items : Dict Id Item
    , name : String
    , buildId : Int
    , currentRoom : RoomId
    , viewing : View
    , log : List String
    , inventory : Set Id
    , mode : Mode
    }


type Mode
    = Building
    | Running
    | Finished


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


type Msg
    = SetView View
    | MoveRoom RoomId Message
    | PickUpItem Id
    | DropItem Id
    | UseItem Id
    | Restart


type alias Name =
    String


type alias Description =
    String



---- FUNCTIONS ----


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Restart ->
            ( game, Browser.Navigation.reload )

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
                                        \_ g -> ( g, "Nothing happens." )
                           )

                ( nextGame, message ) =
                    toolUse (ItemId item) game
            in
            ( addLog message nextGame, Cmd.none )


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


getCurrentRoom : Game -> Room
getCurrentRoom { rooms, currentRoom } =
    case currentRoom of
        RoomId id ->
            Dict.get id rooms |> Maybe.withDefault fallbackRoom



---- FUNCTIONS EXPORTED ALL THE WAY ----


setRoom : RoomId -> Game -> Game
setRoom room game =
    { game | viewing = RoomDescription, currentRoom = room }
