module Game exposing
    ( Game
    , Msg(..)
    , View(..)
    , Item(..)
    , addConnection
    , addRoom
    , addItemToRoom
    , createTool
    , createContainer
    , getCurrentRoom
    , finalize
    , makeGame
    , thingName
    , update
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


type alias Being =
    { description : String
    , name : String
    }


type alias ToolData =
    { description : String
    , name : String
    }


type alias ContainerData =
    { description : String
    , name : String
    , contents : Set Id
    }


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


type alias Game =
    { rooms : Dict Id Room
    , items : Dict Id Item
    , name : String
    , buildId : Int
    , currentRoom : RoomId
    , viewing : View
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
    , description = "Some how you've found yourself in an empty void. You see nothing. You feel nothing. You smell nothing. Your mind starts to crumble under the nothingness."
    , contents = Set.empty
    , connections = []
    }


thingName : { a | name : String } -> String
thingName { name } =
    name


addConnection : { from : RoomId, to : RoomId, name : String, description : String } -> Game -> Game
addConnection { from, to, name, description } ({ rooms } as game) =
    { game
        | rooms =
            Dict.update
                (case from of
                    RoomId id ->
                        id
                )
                (\maybeRoom ->
                    case maybeRoom of
                        Nothing ->
                            Nothing

                        Just ({ connections } as room) ->
                            Just
                                { room
                                    | connections =
                                        { name = name
                                        , description = description
                                        , to = to
                                        , locked = Unlocked
                                        }
                                            :: connections
                                }
                )
                rooms
    }


finalize : RoomId -> Game -> Game
finalize initialRoom game =
    { game
        | currentRoom = initialRoom
    }


type Msg
    = SetView View
    | MoveRoom RoomId


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        SetView v ->
            ( { game | viewing = v }, Cmd.none )

        MoveRoom nextRoom ->
            ( { game | viewing = RoomDescription, currentRoom = nextRoom }, Cmd.none )


createTool : String -> String -> Game -> ( ItemId, Game )
createTool name description ({ buildId, items } as game) =
    let
        item =
            Tool { name = name, description = description }

    in
    ( ItemId buildId
    , { game
        | items =
            Dict.insert buildId item items
        , buildId = buildId + 1
      }
    )


createContainer : String -> String -> Game -> ( ItemId, Game )
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
addItemToRoom (RoomId roomId) ( (ItemId itemId), { rooms } as game ) =
    { game |
        rooms =
            Dict.update
                roomId
                (\maybeRoom ->
                    case maybeRoom of
                        Nothing ->
                            Nothing

                        Just ({ contents } as room) ->
                            Just { room | contents = Set.insert itemId contents }
                )
                rooms
    }
