module Show exposing (..)

-- Show 'sheet music' for a particular song.
--

import Browser
import Html exposing (Html, text, pre, h1, h2, div, img, hr)
import Html.Attributes exposing (href, src, style)
import Http
import Svg exposing (..)
import Svg.Attributes exposing (..)

import List exposing (map,head,concatMap,take,drop,indexedMap)
import String exposing (..)
import String.Format

import RBY
import GSC

import Protocol exposing (Metadata,Song,Note,Key,keyToInt,removeHeader,totalDuration,totalDurations,Channel,headtail,get,intToKey,isBlack)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
--      { url = "../data/gsc/audio/music/postcredits.asm"
--      { url = "../data/gsc/audio/music/kantowildbattle.asm"
--      { url = "../data/gsc/audio/music/kantogymbattle.asm"
--      { url = "../data/gsc/audio/music/johtogymbattle.asm"
--      { url = "../data/gsc/audio/music/goldenrodcity.asm"
      { url = "../data/gsc/audio/music/goldsilveropening.asm"
      , expect = Http.expectString GotText
      }
  )



-- UPDATE


type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success (removeHeader fullText), Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      Html.text "I was unable to load your book."

    Loading ->
      Html.text "Loading..."

    Success fullText ->
      let
        song = (GSC.loadSong { name = "Pokémon GSC - KantoWildBattle"
--        song = (GSC.loadSong { name = "Pokémon GSC - Goldenrod City"
                             , asm = fullText
                             })
        { name, channels } = song
        
        duration = totalDuration song
        (ch0_duration, ch1_duration) = duration

        pianoConfig = { duration = ch0_duration + ch1_duration
                      , min_key = 0
                      , max_key = 100
                      }

        pianoSVG = drawPiano pianoConfig
        pianoHTML = svg
          [ viewBox ("0 0 {{ width }} {{ height }}"
                     |> String.Format.namedValue "width" (fromInt (pianoWidth pianoConfig))
                     |> String.Format.namedValue "height" "3")
--          , width "fill"
--          , Html.Attributes.style "border" "1px solid #c3c3c3"
          -- to get piano on all printed pages, maybe try
          -- https://stackoverflow.com/a/64599494/939402
          ]
          [ pianoSVG
          ]

        backgroundSVG = drawSheetBG pianoConfig
        loopLineSVG = line [ x1 "0"
                           , x2 (fromInt (pianoWidth pianoConfig))
                           , y1 (fromInt ch0_duration)
                           , y2 (fromInt ch0_duration)
                           , stroke "#ffffff"
                           , strokeWidth "2"
                           ] []
        notesSVG = g [] (channels |> indexedMap drawChannel)
        sheetHTML = svg
          [ viewBox ("0 0 {{ width }} {{ height }}"
                     |> String.Format.namedValue "width" (fromInt (pianoWidth pianoConfig))
                     |> String.Format.namedValue "height" (fromInt (sheetHeight pianoConfig)))
          , width "100%"
          , height (fromInt (sheetHeight pianoConfig))
          , preserveAspectRatio "none"
--          , Html.Attributes.style "border" "1px solid #c3c3c3"
--          , opacity "0.4"
          ]
          [ backgroundSVG
          , loopLineSVG
          , notesSVG
          ]

--        center = [ Html.Attributes.style "display" "flex"
--                 , Html.Attributes.style "justify-content" "center"
--                 ]
      in
        div [
            ]
            [ pre [] (Debug.log(Debug.toString song) [])
            , pre [] [ Html.text (Debug.toString (totalDurations song)) ]
            , pre [] [ Html.text (Debug.toString name) ]
            , pianoHTML
            , sheetHTML
            ]

type alias PianoConfig =
  { duration : Int
  , min_key : Int
  , max_key : Int
  }

pianoWidth : PianoConfig -> Int
pianoWidth {min_key, max_key} = max_key - min_key

sheetHeight : PianoConfig -> Int
sheetHeight {duration} = duration

drawPiano : PianoConfig -> Svg msg
drawPiano { min_key, max_key } =
  let
    keysSVG =
      range min_key max_key
      |> List.map (\key -> rect [ x (fromInt (key - min_key))
                                , y "0"
                                , width "1"
                                , height "3"
                                , fill (if isBlack (intToKey (key - min_key))
                                        then "#000000" else "#ffffff")
                                ] [])
    sepsSVG =
      range (min_key + 1) max_key
      |> List.map (\key -> line [ x1 (fromInt (key - min_key))
                                , x2 (fromInt (key - min_key))
                                , y1 "0"
                                , y2 "3"
                                , stroke "#c0c0c0"
                                , strokeWidth ".1"
                                ] [])
  in
    g [] (keysSVG ++ sepsSVG)

range : Int -> Int -> List Int
range a b = List.range a (b - 1)

drawSheetBG : PianoConfig -> Svg msg
drawSheetBG {duration, min_key, max_key} =
  let
    keysSVG =
      range min_key max_key
      |> List.map (\key -> rect [ x (fromInt (key - min_key))
                                , y "0"
                                , width "1"
                                , height (fromInt duration)
                                , fill (if isBlack (intToKey (key - min_key))
                                        then "#e0e0e0" else "#ffffff")
                                ] [])
    sepsSVG =
      range (min_key + 1) max_key
      |> List.map (\key -> line [ x1 (fromInt (key - min_key))
                                , x2 (fromInt (key - min_key))
                                , y1 "0"
                                , y2 (fromInt duration)
                                , stroke "#f0f0f0"
                                , strokeWidth ".1"
                                ] [])
    hrulesSVG =
      range 0 (duration // 48)
      |> List.map (\mea -> line [ x1 "0"
                                , x2 (fromInt (max_key - min_key))
                                , y1 (fromInt (mea * 48))
                                , y2 (fromInt (mea * 48))
                                , stroke "#f0f0f0"
                                , strokeWidth "1"
                                ] [])
  in
    g [] (keysSVG ++ sepsSVG ++ hrulesSVG)

--type alias Color = String
colors = ["red", "green", "blue"]
--colors = ["#ff0000", "#008000", "#0000ff", "#ffa500"];

drawChannel : Int -> Channel -> Svg msg
drawChannel chanId channel =
  let
    {boot, loop} = channel
    (drawA, i) = drawBlock (boot, 0)
    (drawB, j) = drawBlock (loop, i)
    color = get chanId colors
  in
    g [fill color, stroke color] (List.append drawA drawB)

drawBlock : (List Note, Int) -> (List (Svg msg), Int)
drawBlock (notes, i) =
  case notes of
    [] -> ([], i)
    note :: notes_ ->
      let
        (note_, i_) = drawNote (note, i)
        (note__, i__) = drawBlock (notes_, i_)
      in
        (List.append note_ note__, i__)
       

drawNote : (Note, Int) -> (List (Svg msg), Int)
drawNote ({ duration, what, playedBefore }, i) = case what of
  Nothing -> ([], i + duration)
  Just { key, legato, strength } ->
    let
      head = line [ x1 (fromInt key)
              , x2 (fromInt (key + 1))
              , y1 (fromInt i)
              , y2 (fromInt i)
              , strokeWidth "2"
              , strokeOpacity "1"
              ] []

      body = {-if legato <= 2
             then rect [ x (fromInt key)
                       , y (fromInt i)
                       , width "1"
                       , height (fromInt duration)
                       , fill "url(#grad1)"
                       , fillOpacity "0.625"
                       , strokeWidth "0"
                       ] []
             else -} rect [ x (fromInt key)
                       , y (fromInt i)
                       , width "1"
                       , height (fromInt duration)
                       , fillOpacity (if playedBefore then "0.4" else "0.625")
                       , strokeWidth "0"
                       ] []
      gradient_def =
        defs [] [linearGradient
          [ id "grad1",x1 "0%",y1 "0%",x2 "0%",y2 "100%" ]
          [ stop [offset "0%",Svg.Attributes.style "stop-opacity:1"] []
            , stop [offset "75%",Svg.Attributes.style "stop-opacity:0"] []
          ]]
    in
      ([ gradient_def
       , body 
       , head
       ]
       , i + duration)









