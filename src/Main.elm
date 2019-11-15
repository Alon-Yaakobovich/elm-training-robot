module Main exposing (Model, Msg, subscriptions, update, view)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (button, div, h2, input, strong, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events exposing (onClick, onInput)
import Keyboard exposing (Key(..))
import Robot exposing (..)
import Time exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always subscriptions
        }


type alias Model =
    { robot1 : Robot.Model
    , robot2 : Robot.Model
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { robot1 =
            { pressedKeys = []
            , robotData = Robot.setRobotData 1 1 False
            , shotData = Robot.setShotData 0 0 190 188 False False
            , robotButtons = Robot.setRobotButtons Shift True
            }
      , robot2 =
            { pressedKeys = []
            , robotData = Robot.setRobotData 4 4 False
            , shotData = Robot.setShotData 0 0 69 81 False False
            , robotButtons = Robot.setRobotButtons Spacebar False
            }
      }
    , Cmd.none
    )


type alias Msg =
    Robot.Msg


view : Model -> Html.Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "width" "90%"
        , style "height" "90%"
        ]
        [ Robot.robotView model.robot1 "red"
        , Robot.robotView model.robot2 "blue"
        , Robot.shotView model.robot1 "red"
        , Robot.shotView model.robot2 "blue"
        ]


update : Msg -> Model -> Model
update msg model =
    { model
        | robot1 = Robot.update msg model.robot1
        , robot2 = Robot.update msg model.robot2
    }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Time.every 10 ChangeShotPos
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]
