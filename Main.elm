module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Task
import String
import Time
import Binary
import Json.Decode as Json

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { timestamp : Int
    , command : String
    }
  
init : () -> (Model, Cmd Msg)
init _ =
  ( { timestamp = 300
    , command = ""
    } , Cmd.none )

  
type Msg
  = Tick Time.Posix
  | RecordCommand String
  | KeyDown Int
  | Refocus
  | Noop

focusCommandBox : Cmd Msg
focusCommandBox =
    Task.attempt (\_ -> Noop) ( Dom.focus "txtCommand" )

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

{-
Commands:
  5 min/hour/m/h/hr
  add 5 min/hour/h/m/hr
  remove 5 min
  minus 5 min
  5 min
  -5 min
  +5 -> default to min
  -5
  5
  show clock
  show list
  show history
  history
  list
-}
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick tick -> 
        if model.timestamp > 0 then
            ( { model | timestamp = model.timestamp - 1 }, Cmd.none )
        else
            ( model, Cmd.none )

    RecordCommand value ->
        ( { model | command = value }, Cmd.none )

    KeyDown keycode ->
        if keycode == 13 then
            ( { model | timestamp = model.timestamp + 300 }, Cmd.none )
        else
            ( model, Cmd.none )

    Refocus ->
        ( model, focusCommandBox )

    Noop ->
        ( model, Cmd.none )
            
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timestamp > 0 then
        Time.every 1000 Tick
    else
        Sub.none

secToHMS : Int -> ( Int, Int, Int )
secToHMS time =
    let
        h = time // 3600
        rm = modBy 3600 time
        m = rm // 60
        s = modBy 60 rm
    in
        (h, m, s)
    
toBits : Int -> List Int
toBits a = Binary.fromDecimal a |> Binary.ensureSize 4 |> Binary.toIntegers

bitToDiv : Int -> Int -> Html Msg
bitToDiv n i = li [ classList [ ( "bit-dots", True )
                              , ( "on", i == 1 )
                              ]
                 ] [ ]
             
bitsDot : List Int -> List ( Html Msg )
bitsDot bits = List.indexedMap bitToDiv bits

renderDigit : Int -> Html Msg
renderDigit n =
    div [ class "bits-group" ]
        [ bitsDot ( toBits n ) |> ul []
        ]

renderNumber : Int -> Html Msg
renderNumber n =
    let
        l = n // 10
        r = modBy 10 n
    in
        div [ class "number-block" ]
            [ renderDigit l
            , renderDigit r
            ]

padTime : Int -> String
padTime n =
    String.padLeft 2 '0' ( String.fromInt n )

renderTimeString : Int -> Int -> Int -> String
renderTimeString h m s =
    padTime h ++ ":" ++ padTime m ++ ":" ++ padTime s
            
view : Model -> Html Msg
view model =
  let
      (h, m, s) = secToHMS model.timestamp
  in
      div [ class "container" ]
          [ div [ class "numbers-group" ]
                [ renderNumber h
                , renderNumber m
                , renderNumber s
                ]
          , h2 [ class "time-string" ] [ text ( renderTimeString h m s ) ]
          , div [ class "command-box" ]
              [ input [ autofocus True
                      , id "txtCommand"
                      , onInput RecordCommand
                      , onBlur Refocus
                      , onKeyDown KeyDown
                      , placeholder "No command implemented yet, just press ENTER to add 5 minutes"
                      ] []
              ]
          ]
          
