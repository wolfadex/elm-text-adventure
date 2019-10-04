module Game.View exposing (program)

import Browser exposing (Document)
import Dict
import Set
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game, Msg(..), View(..), Item(..))


program : Game -> Program () Game Game.Msg
program game =
    Browser.document
        { init = \_ -> ( game, Cmd.none )
        , view = view
        , update = Game.update
        , subscriptions = \_ -> Sub.none    
        }


view : Game -> Document Msg
view game =
    { title = game.name
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color <| Element.rgb 0.1 0.1 0.1
            ]
        <|
            viewGame game
        ]
    }


viewGame : Game -> Element Msg
viewGame game =
    Element.column
        [ Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum (scaled 17)
            )
        , Element.height Element.fill
        , Border.shadow
            { offset = ( 2, 2 )
            , size = 1
            , blur = 8
            , color = Element.rgba 0 0 0 0.5
            }
        , Background.color <| Element.rgb 0.15 0.15 0.15
        , Font.color <| Element.rgb 0.8 0.8 0.8
        ]
        [ Element.wrappedRow
            [ Element.spacing <| scaled 1
            , Element.padding <| scaled 1
            ]
            [ button "Describe Room" (SetView RoomDescription)
            , button "List Inventory" (SetView PersonInventory)
            , button "Items in Room" (SetView RoomInventory)
            , button "Exits" (SetView RoomExits)
            ]
        , spacer
        , game
            |> Game.getCurrentRoom
            |> Game.thingName
            |> Element.text
            |> Element.el
                [ Font.underline
                , Element.padding <| scaled 1
                ]
        , spacer
        , game
            |> .viewing
            |> (\v ->
                    case v of
                        RoomDescription ->
                            game
                                |> Game.getCurrentRoom
                                |> .description
                                |> Element.text
                                |> List.singleton
                                |> Element.paragraph
                                    [ Element.padding (scaled 1) ]

                        RoomExits ->
                            game
                                |> Game.getCurrentRoom
                                |> .connections
                                |> List.map
                                    (\{ name, description, to, message } ->
                                        Element.wrappedRow
                                            []
                                            [ button name (MoveRoom to message)
                                            , buttonSpacer
                                            , Element.text description
                                            ]
                                    )
                                |> Element.column
                                    [ Element.spacing (scaled 1)
                                    , Element.padding (scaled 1)
                                    ]

                        RoomInventory ->
                            game
                                |> Game.getCurrentRoom
                                |> .contents
                                |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
                                |> Dict.toList
                                |> List.map
                                    (\( id, item ) ->
                                        let
                                            ( n, d ) =
                                                case item of
                                                    Tool { name, description } ->
                                                        ( name, description )

                                                    Container { name, description } ->
                                                        ( name, description )
                                        in
                                        Element.wrappedRow
                                            []
                                            [ button n (PickUpItem id)
                                            , buttonSpacer
                                            , Element.text d
                                            ]
                                    )
                                |> Element.column
                                    [ Element.spacing (scaled 1)
                                    , Element.padding (scaled 1)
                                    ]

                        PersonInventory ->
                            game
                                |> .inventory
                                |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
                                |> Dict.toList
                                |> List.map
                                    (\( id, item ) ->
                                        let
                                            ( n, d ) =
                                                case item of
                                                    Tool { name, description } ->
                                                        ( name, description )

                                                    Container { name, description } ->
                                                        ( name, description )
                                        in
                                        Element.wrappedRow
                                            []
                                            [ button "Drop" (DropItem id)
                                            , buttonSpacer
                                            , button "Use" (UseItem id)
                                            , buttonSpacer
                                            , Element.text <| n ++ ":"
                                            , buttonSpacer
                                            , Element.text d
                                            ]
                                    )
                                |> Element.column
                                    [ Element.spacing (scaled 1)
                                    , Element.padding (scaled 1)
                                    ]
                )
        , spacer
        , game
            |> .log
            |> List.map (\log -> Element.paragraph [] [ Element.text log ])
            |> List.intersperse
                (Element.el
                    [ Element.width Element.fill
                    , Border.width 1
                    , Border.color (Element.rgb 0.4 0.4 0.4)
                    ]
                    Element.none
                )
            |> Element.column
                [ Element.spacing <| scaled 1
                , Element.padding <| scaled 1
                , Element.scrollbarY
                ]

        ]


spacer : Element msg
spacer =
    Element.el
        [ Border.color (Element.rgb 0.07 0.07 0.07)
        , Border.width 3
        , Element.width Element.fill
        ]
        Element.none


button : String -> Msg -> Element Msg
button label action =
    Input.button
        [ Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 2
            --, color = Element.rgb 0.4 0.4 0.8
            , color = Element.rgba 1 1 1 0.5
            }
        , Element.paddingXY 3 1
        --, Background.color (Element.rgb 1 1 1)
        , Background.color (Element.rgb 0.4 0.4 0.8)
        --, Font.color (Element.rgb 0.2 0.2 0.2)
        ]
        { onPress = Just action
        , label = Element.text label
        }


buttonSpacer : Element msg
buttonSpacer =
    Element.el
        [ Element.width <| Element.px (scaled 1) ]
        Element.none


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor
