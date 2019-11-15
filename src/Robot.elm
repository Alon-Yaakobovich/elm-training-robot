module Robot exposing (..)

import Browser
import Browser.Events as Events exposing (..)
import Html exposing (button, div, h2, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Json.Decode
import Keyboard exposing (..)
import Keyboard.Arrows exposing (..)
import Time exposing (..)


type alias RobotData =
    { xPos : Int
    , yPos : Int
    , toggleSpeed : Bool
    }


setRobotData : Int -> Int -> Bool -> RobotData
setRobotData xPosition yPosition toggleSpeed =
    { xPos = xPosition
    , yPos = yPosition
    , toggleSpeed = toggleSpeed
    }


type alias ShotData =
    { xPos : Int
    , yPos : Int
    , keyShootRight : Int
    , keyShootLeft : Int
    , isShotExist : Bool
    , isShootingRght : Bool
    }


setShotData : Int -> Int -> Int -> Int -> Bool -> Bool -> ShotData
setShotData xPosition yPosition keyShootRight keyShootLeft isShotExist isShootingRght =
    { xPos = xPosition
    , yPos = yPosition
    , keyShootRight = keyShootRight
    , keyShootLeft = keyShootLeft
    , isShotExist = isShotExist
    , isShootingRght = isShootingRght
    }


type alias RobotButtons =
    { keySprint : Keyboard.Key
    , useArrows : Bool
    }


setRobotButtons : Keyboard.Key -> Bool -> RobotButtons
setRobotButtons keySprint useArrows =
    { keySprint = keySprint
    , useArrows = useArrows
    }


type alias Model =
    { pressedKeys : List Keyboard.Key
    , robotData : RobotData
    , shotData : ShotData
    , robotButtons : RobotButtons
    }


type Msg
    = ChangeShotPos Time.Posix
    | KeyboardMsg Keyboard.Msg


robotView : Model -> String -> Html.Html Msg
robotView model color =
    div
        [ style "position" "absolute"
        , style "background-color" color
        , style "width" "10px"
        , style "height" "10px"
        , style "left" <| String.fromInt model.robotData.xPos ++ "%"
        , style "top" <| String.fromInt model.robotData.yPos ++ "%"
        ]
        [ text "Robot" ]


shotView : Model -> String -> Html.Html Msg
shotView model color =
    let
        shotColor : String
        shotColor =
            if model.shotData.isShotExist == True then
                color

            else
                "white"
    in
    div
        [ style "position" "absolute"
        , style "width" "5px"
        , style "height" "5px"
        , style "left" ((++) (String.fromInt model.shotData.xPos) "%")
        , style "top" ((++) (String.fromInt model.shotData.yPos) "%")
        , style "background-color" shotColor
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            let
                newPressedKeys : List Keyboard.Key
                newPressedKeys =
                    Keyboard.updateWithParser Keyboard.anyKeyUpper keyMsg model.pressedKeys

                chooseSpeed : Int
                chooseSpeed =
                    if List.member model.robotButtons.keySprint model.pressedKeys == True then
                        3

                    else
                        1

                robotSpeed : { x : Int, y : Int }
                robotSpeed =
                    if model.robotButtons.useArrows == True then
                        Keyboard.Arrows.arrows model.pressedKeys

                    else
                        Keyboard.Arrows.wasd model.pressedKeys

                robotNewXPos : Int
                robotNewXPos =
                    model.robotData.xPos + robotSpeed.x * chooseSpeed

                robotNewYPos : Int
                robotNewYPos =
                    model.robotData.yPos - robotSpeed.y * chooseSpeed
            in
            { model
                | pressedKeys = newPressedKeys
                , robotData =
                    { xPos = clamp 0 100 <| robotNewXPos
                    , yPos = clamp 0 100 <| robotNewYPos
                    , toggleSpeed = model.robotData.toggleSpeed
                    }
            }

        _ ->
            model


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Time.every 10 ChangeShotPos
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]
