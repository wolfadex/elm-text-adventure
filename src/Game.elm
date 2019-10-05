module Game exposing
    ( Game
    , Msg
    , addConnection
    , addItemToRoom
    , addRoom
    , createContainer
    , createTool
    , finalize
    , getCurrentRoom
    , makeGame
    , thingName
    , setRoom
    , deleteItem
    )

import Dict exposing (Dict)
import Set exposing (Set)
import Game.Internal exposing
    ( Msg(..)
    , Item(..)
    , View(..)
    , ItemUse
    , ItemId(..)
    , RoomId(..)
    , Game
    , Message
    , Locked(..)
    , Room
    , Name
    , Description
    )


--type alias Being =
--    { description : String
--    , name : String
--    }

type alias Game =
    Game.Internal.Game


type alias Msg =
    Game.Internal.Msg


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
            Dict.get id rooms |> Maybe.withDefault Game.Internal.fallbackRoom


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
setRoom =
    Game.Internal.setRoom


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
