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
    , decodeGame
    , encodeGame
    , fallbackRoom
    , finishGame
    , getCurrentRoom
    , setRoom
    , startGame
    , update
    )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode exposing (Value)
import Set exposing (Set)



---- TYPES ----


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


encodeRoom : ( Id, Room ) -> ( String, Value )
encodeRoom ( id, room ) =
    ( String.fromInt id
    , Json.Encode.object
        [ ( "description", Json.Encode.string room.description )
        , ( "name", Json.Encode.string room.name )
        , ( "contents"
          , room.contents
                |> Set.toList
                |> Json.Encode.list Json.Encode.int
          )
        , ( "connections"
          , room.connections
                |> Json.Encode.list encodeConnection
          )
        ]
    )


decodeRoom : Decoder Room
decodeRoom =
    Json.Decode.map4 Room
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "contents" (decodeSet Json.Decode.int))
        (Json.Decode.field "connections" (Json.Decode.list decodeConnection))


type ItemId
    = ItemId Id


type Item
    = Tool ToolData
    | Container ContainerData


encodeItem : ( Id, Item ) -> ( String, Value )
encodeItem ( id, item ) =
    ( String.fromInt id
    , Json.Encode.object
        [ ( "type"
          , Json.Encode.string <|
                case item of
                    Tool _ ->
                        "tool"

                    Container _ ->
                        "container"
          )
        , ( "data"
          , case item of
                Tool data ->
                    encodeTool data

                Container data ->
                    encodeContainer data
          )
        ]
    )


decodeItem : Dict String ItemUse -> Decoder Item
decodeItem toolUseBuilder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\itemType ->
                case itemType of
                    "tool" ->
                        Json.Decode.field "data" (decodeTool toolUseBuilder) |> Json.Decode.andThen (Tool >> Json.Decode.succeed)

                    "container" ->
                        Json.Decode.field "data" decodeContainer |> Json.Decode.andThen (Container >> Json.Decode.succeed)

                    _ ->
                        Json.Decode.fail ("Unknown item type of: " ++ itemType)
            )


type alias ToolData =
    { description : String
    , name : String
    , use : ItemUse
    , decoderKey : String
    }


encodeTool : ToolData -> Value
encodeTool { description, name, decoderKey } =
    Json.Encode.object
        [ ( "description", Json.Encode.string description )
        , ( "name", Json.Encode.string name )
        , ( "decoderKey", Json.Encode.string decoderKey )
        ]


decodeTool : Dict String ItemUse -> Decoder ToolData
decodeTool toolUseBuilder =
    Json.Decode.map3
        (\description name decoderKey ->
            { description = description
            , name = name
            , decoderKey = decoderKey
            , use =
                toolUseBuilder
                    |> Dict.get decoderKey
                    |> Maybe.withDefault (\_ g -> ( g, "You've forgotten to implement the tool use decoder for the item: " ++ decoderKey ))
            }
        )
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "decoderKey" Json.Decode.string)


type alias ContainerData =
    { description : String
    , name : String
    , contents : Set Id
    }


encodeContainer : ContainerData -> Value
encodeContainer { description, name, contents } =
    Json.Encode.object
        [ ( "description", Json.Encode.string description )
        , ( "name", Json.Encode.string name )
        , ( "contents"
          , contents
                |> Set.toList
                |> Json.Encode.list Json.Encode.int
          )
        ]


decodeContainer : Decoder ContainerData
decodeContainer =
    Json.Decode.map3 ContainerData
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "contents" (decodeSet Json.Decode.int))


type alias ItemUse =
    ItemId -> Game -> ( Game, Message )


type Game
    = Building BuildData
    | Running RunData
    | Finished FinishData


encodeGame : Game -> Value
encodeGame game =
    Json.Encode.object
        [ ( "mode"
          , Json.Encode.string <|
                case game of
                    Building _ ->
                        "building"

                    Running _ ->
                        "running"

                    Finished _ ->
                        "finished"
          )
        , ( "data"
          , case game of
                Building data ->
                    encodeGameBuilding data

                Running data ->
                    encodeGameRunning data

                Finished data ->
                    encodeGameFinished data
          )
        ]


decodeGame : Dict String ItemUse -> Decoder Game
decodeGame toolUseBuilder =
    Json.Decode.field "mode" Json.Decode.string
        |> Json.Decode.andThen
            (\mode ->
                case mode of
                    "building" ->
                        Json.Decode.field "data" (decodeGameBuilding toolUseBuilder)

                    "running" ->
                        Json.Decode.field "data" (decodeGameRunning toolUseBuilder)

                    "finished" ->
                        Json.Decode.field "data" (decodeGameFinished toolUseBuilder)

                    _ ->
                        Json.Decode.fail ("Unknown mode: " ++ mode ++ ", Unabled to decode game.")
            )


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


encodeGameBuilding : BuildData -> Value
encodeGameBuilding data =
    let
        (RoomId roomId) =
            data.currentRoom
    in
    Json.Encode.object
        [ ( "rooms"
          , data.rooms
                |> Dict.toList
                |> List.map encodeRoom
                |> Json.Encode.object
          )
        , ( "items"
          , data.items
                |> Dict.toList
                |> List.map encodeItem
                |> Json.Encode.object
          )
        , ( "name", Json.Encode.string data.name )
        , ( "buildId", Json.Encode.int data.buildId )
        , ( "currentRoom", Json.Encode.int roomId )
        , ( "log", Json.Encode.list Json.Encode.string data.log )
        , ( "inventory"
          , data.inventory
                |> Set.toList
                |> Json.Encode.list Json.Encode.int
          )
        , ( "theme", encodeTheme data.theme )
        ]


decodeGameBuilding : Dict String ItemUse -> Decoder Game
decodeGameBuilding toolUseBuilder =
    decodeBuildData toolUseBuilder
        |> Json.Decode.andThen (Building >> Json.Decode.succeed)
    


decodeBuildData : Dict String ItemUse -> Decoder BuildData
decodeBuildData toolUseBuilder =
    Json.Decode.succeed BuildData
        |> required "rooms" (decodeDictId decodeRoom)
        |> required "items" (decodeDictId (decodeItem toolUseBuilder))
        |> required "name" Json.Decode.string
        |> required "buildId" Json.Decode.int
        |> required "currentRoom" decodeRoomId
        |> required "log" (Json.Decode.list Json.Decode.string)
        |> required "inventory" (decodeSet Json.Decode.int)
        |> required "theme" decodeTheme


decodeSet : Decoder comparable -> Decoder (Set comparable)
decodeSet comparableDecoder =
    Json.Decode.list comparableDecoder
        |> Json.Decode.andThen (Set.fromList >> Json.Decode.succeed)


decodeDictId : Decoder a -> Decoder (Dict Id a)
decodeDictId valueDecoder =
    Json.Decode.dict valueDecoder
        |> Json.Decode.andThen
            (\dictOfA ->
                Json.Decode.succeed <|
                    Dict.foldl
                        (\k v r ->
                            case String.toInt k of
                                Just kId ->
                                    Dict.insert kId v r

                                Nothing ->
                                    r
                        )
                        Dict.empty
                        dictOfA
            )


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


encodeGameRunning : RunData -> Value
encodeGameRunning data =
    let
        (RoomId roomId) =
            data.currentRoom
    in
    Json.Encode.object
        [ ( "rooms"
          , data.rooms
                |> Dict.toList
                |> List.map encodeRoom
                |> Json.Encode.object
          )
        , ( "items"
          , data.items
                |> Dict.toList
                |> List.map encodeItem
                |> Json.Encode.object
          )
        , ( "name", Json.Encode.string data.name )
        , ( "buildId", Json.Encode.int data.buildId )
        , ( "currentRoom", Json.Encode.int roomId )
        , ( "log", Json.Encode.list Json.Encode.string data.log )
        , ( "inventory"
          , data.inventory
                |> Set.toList
                |> Json.Encode.list Json.Encode.int
          )

        -- No need to encode these
        --, descriptionDetail : Detail
        --, exitsDetail : Detail
        --, roomItemsDetail : Detail
        --, inventoryDetail : Detail
        , ( "theme", encodeTheme data.theme )
        , ( "initialGame", encodeGameBuilding data.initialGame )
        ]


decodeGameRunning : Dict String ItemUse -> Decoder Game
decodeGameRunning toolUseBuilder =
    decodeRunData toolUseBuilder
        |> Json.Decode.andThen (Running >> Json.Decode.succeed)


decodeRunData : Dict String ItemUse -> Decoder RunData
decodeRunData toolUseBuilder =
    Json.Decode.succeed RunData
        |> required "rooms" (decodeDictId decodeRoom)
        |> required "items" (decodeDictId (decodeItem toolUseBuilder))
        |> required "name" Json.Decode.string
        |> required "buildId" Json.Decode.int
        |> required "currentRoom" decodeRoomId
        |> required "log" (Json.Decode.list Json.Decode.string)
        |> required "inventory" (decodeSet Json.Decode.int)
        |> hardcoded Collapsed
        |> hardcoded Collapsed
        |> hardcoded Collapsed
        |> hardcoded Collapsed
        |> required "theme" decodeTheme
        |> required "initialGame" (decodeBuildData toolUseBuilder)


type alias FinishData =
    { log : List String
    , theme : Theme
    , initialGame : BuildData
    }


encodeGameFinished : FinishData -> Value
encodeGameFinished data =
    Json.Encode.object
        [ ( "log", Json.Encode.list Json.Encode.string data.log )
        , ( "theme", encodeTheme data.theme )
        , ( "initialGame", encodeGameBuilding data.initialGame )
        ]


decodeGameFinished : Dict String ItemUse -> Decoder Game
decodeGameFinished toolUseBuilder =
    decodeFinishData toolUseBuilder
        |> Json.Decode.andThen (Finished >> Json.Decode.succeed)


decodeFinishData : Dict String ItemUse -> Decoder FinishData
decodeFinishData toolUseBuilder =
    Json.Decode.succeed FinishData
        |> required "log" (Json.Decode.list Json.Decode.string)
        |> required "theme" decodeTheme
        |> required "initialGame" (decodeBuildData toolUseBuilder)


type Theme
    = Light
    | Dark


encodeTheme : Theme -> Value
encodeTheme theme =
    Json.Encode.string <|
        case theme of
            Light ->
                "light"

            Dark ->
                "dark"


decodeTheme : Decoder Theme
decodeTheme =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                Json.Decode.succeed <|
                    if str == "dark" then
                        Dark

                    else
                        Light
            )


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


encodeConnection : Connection -> Value
encodeConnection connection =
    let
        (RoomId roomId) =
            connection.to
    in
    Json.Encode.object
        [ ( "to", Json.Encode.int roomId )
        , ( "locked"
          , encodeLocked connection.locked
          )
        , ( "name", Json.Encode.string connection.name )
        , ( "description", Json.Encode.string connection.description )
        , ( "message", Json.Encode.string connection.message )
        ]


decodeConnection : Decoder Connection
decodeConnection =
    Json.Decode.map5 Connection
        (Json.Decode.field "to" decodeRoomId)
        (Json.Decode.field "locked" decodeLocked)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)


decodeRoomId : Decoder RoomId
decodeRoomId =
    Json.Decode.int
        |> Json.Decode.andThen (RoomId >> Json.Decode.succeed)


type Locked
    = Locked
    | Unlocked


encodeLocked : Locked -> Value
encodeLocked locked =
    Json.Encode.bool <|
        case locked of
            Locked ->
                True

            Unlocked ->
                False


decodeLocked : Decoder Locked
decodeLocked =
    Json.Decode.bool
        |> Json.Decode.andThen
            (\locked ->
                Json.Decode.succeed <|
                    if locked then
                        Locked

                    else
                        Unlocked
            )


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
                            ( Finished (addLog message d), Cmd.none )

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
