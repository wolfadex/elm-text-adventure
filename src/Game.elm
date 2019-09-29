module Game exposing
    ( Game
    , Msg(..)
    , addConnection
    , addRoom
    , describe
    , getConnections
    , getCurrentRoom
    , init
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
    , currentDescription : String
    }


type alias Connection =
    { to : RoomId
    , locked : Locked
    , name : String
    , description : String
    }


type Locked
    = Locked
    | Unlocked


describe : { a | description : String } -> String
describe { description } =
    description


makeGame : Name -> Game
makeGame name =
    { rooms = Dict.empty
    , items = Dict.empty
    , name = name
    , buildId = 0
    , currentRoom = RoomId -1
    , currentDescription = ""
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


getConnections : { a | connections : List Connection } -> List Connection
getConnections { connections } =
    connections


init : RoomId -> Game -> Game
init initialRoom game =
    let
        gameWithInitialRoom =
            { game
                | currentRoom = initialRoom
            }
    in
    { gameWithInitialRoom
        | currentDescription =
            gameWithInitialRoom
                |> getCurrentRoom
                |> describe
    }


type Msg
    = DescribeRoom
    | ListInventory
    | ItemsInRoom


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    ( game, Cmd.none )
