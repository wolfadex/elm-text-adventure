module Game.View exposing (program)

import Browser exposing (Document)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game, Msg(..), View(..))


program : String -> Game -> Program () Game Game.Msg
program name init =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , view = view name
        , update = Game.update
        , subscriptions = \_ -> Sub.none    
        }


view : String -> Game -> Document Msg
view name game =
    { title = name
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
        , Border.shadow
            { offset = ( 2, 2 )
            , size = 1
            , blur = 8
            , color = Element.rgba 0 0 0 0.5
            }
        , Background.color <| Element.rgb 0.15 0.15 0.15
        , Font.color <| Element.rgb 0.8 0.8 0.8
        ]
        [ game
            |> Game.getCurrentRoom
            |> Game.thingName
            |> Element.text
            |> Element.el
                [ Font.underline
                , Element.padding <| scaled 1
                ]
        , Element.el
            [ Border.color (Element.rgb 0 0 0)
            , Border.width 1
            , Element.width Element.fill
            ]
            Element.none
        , game
            |> .viewing
            |> (\v ->
                    case v of
                        RoomDescription ->
                            game
                                |> Game.getCurrentRoom
                                |> Game.describe
                                |> Element.text
                                |> List.singleton
                                |> Element.paragraph
                                    [ Element.padding (scaled 1) ]

                        RoomExits ->
                            game
                                |> Game.getCurrentRoom
                                |> .connections
                                |> List.map
                                    (\{ name, description, to } ->
                                        Element.wrappedRow
                                            []
                                            [ button name (MoveRoom to)
                                            , Element.text description
                                            ]
                                    )
                                |> Element.column
                                    [ Element.spacing (scaled 1)
                                    , Element.padding (scaled 1)
                                    ]

                        _ ->
                            Debug.todo (Debug.toString v)
                )
        , Element.el
            [ Border.color (Element.rgb 0 0 0)
            , Border.width 1
            , Element.width Element.fill
            ]
            Element.none
        , Element.wrappedRow
            [ Element.spacing <| scaled 1
            , Element.padding <| scaled 1
            ]
            [ button "Describe Room" (SetView RoomDescription)
            , button "List Inventory" (SetView PersonInventory)
            , button "Items in Room" (SetView RoomInventory)
            , button "Exits" (SetView RoomExits)
            ]
        ]


button : String -> Msg -> Element Msg
button label action =
    Input.button
        []
        { onPress = Just action
        , label = Element.text ("[ " ++ label ++ " ]")
        }


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor
