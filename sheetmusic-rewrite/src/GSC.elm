module GSC exposing (..)

-- Load all GSC song metadata.
--

import String exposing (startsWith,slice,lines,dropLeft,endsWith,contains,toInt,words)
import Html exposing (Html, text, pre, h1, h2, div, img, hr)
import Html.Attributes exposing (href, src, width)
import List exposing (map,filter,indexedMap,drop,partition,take,drop,isEmpty,append,concat,repeat,range)
import Tuple exposing (first,second)
import Array exposing (Array)

import Protocol exposing (Metadata,Song,Channel,Note,Key,keyToInt,parseKey,get,toInt_,headtail,index,transpose,array_get,totalDuration,cumsum)

parseMetadata : String -> Metadata
parseMetadata fullText =
  { title = "Pokémon Gold, Silver, Crystal"
  , covers = ["../data/gsc/cover/gold.png"
             ,"../data/gsc/cover/silver.png"
             ,"../data/gsc/cover/crystal.jpg"
             ]
  , songs = fullText |> extractSongPaths |> map figureOutAuxiliaryFields
  }

figureOutAuxiliaryFields : String -> {name : String, thumb : String, addr : String}
figureOutAuxiliaryFields path =
  let
    name = slice 12 -4 path
  in
    { name = name
    , thumb = "../data/gsc/thumb/" ++ name ++ ".png"
    , addr = "./show.html?/data/gsc/" ++ path
    }


extractSongPaths : String -> List String
extractSongPaths fullText = fullText
  |> lines
  |> filterDiff (\line -> if startsWith "SECTION \"Songs" line then Start
                          else if startsWith "SECTION" line then Stop
                          else Null)
  |> filter (startsWith "INCLUDE")
  |> map (slice 9 -1)

--[ Generic code to select certain parts of a file
type Diff = Stop | Null | Start
updateState : Diff -> Bool -> Bool
updateState diff state = case diff of
  Stop -> False
  Null -> state
  Start -> True
introduceState : state -> (a -> state -> state) -> List a -> List (state, a)
introduceState s f xs = case xs of
  [] -> []
  x :: xs_ -> let s_ = f x s in (s_, x) :: introduceState s_ f xs_
filterDiff : (a -> Diff) -> List a -> List a
filterDiff f xs = xs
  |> introduceState False (updateState << f)
  |> filter first
  |> map second
--]



-- Load GSC midi data.
--

type alias AsmChannel = List String

loadSong : { name : String, asm : String } -> Song
loadSong { name, asm } =
  let
    asmLines = lines asm |> filter (not << String.isEmpty)

    channels =
      asmLines
      |> listIndexes (\line -> startsWith "Music_" line && slice -5 -2 line == "_Ch") --contains "_Ch" line && endsWith ":" line)
      |> intoBlocks asmLines
      |> filter (listIndexes (contains "drum_note") >> isEmpty)
      |> map simulateChannel
      |> lowerCh3  -- (why?)
      |> handleSongExceptions name

    (t0, t1) = totalDuration channels
    measures =
      if name == "/data/gsc/audio/music/goldsilveropening.asm"
      then cumsum (repeat 24 4 ++ repeat 19 6 ++ repeat 8 4) |> map (\x -> x * 12)
      else range 0 ((t0 + t1) // 48) |> map (\x -> x * 48 + modBy 48 -t0)
  in
    { name = name
    , channels = channels
    , measures = measures
    }

handleSongExceptions : String -> List Channel -> List Channel
handleSongExceptions name channels =
  if name == "/data/gsc/audio/music/goldenrodcity.asm"
  then case channels of
    [ch1, ch2, ch3] ->
      [ch1, { boot = [], loop = ch2.loop ++ take 22 ch2.loop }, { boot = [], loop = ch3.loop ++ take 34 ch3.loop }]
    _ -> Debug.todo "goldenrodcity error"
  else channels
    
lowerCh3 : List Channel -> List Channel
lowerCh3 channels =
  case channels of
    [ch1, ch2, ch3] -> [ch1, ch2, transpose -12 ch3]
    _ -> channels

listIndexes : (a -> Bool) -> List a -> List Int
listIndexes f xs =
  xs
  |> indexedMap Tuple.pair
  |> filter (\(i, a) -> f a)
  |> map first

intoBlocks : List a -> List Int -> List (List a)
intoBlocks xs ix = case ix of
  i0 :: i1 :: ix_ -> take (i1 - i0) (drop i0 xs) :: intoBlocks xs (i1 :: ix_)
  [i] -> [drop i xs]
  [] -> Debug.todo "intoBlocks failed"

type alias MachineState =
  -- note modifiers
  { octave : Int
  , transpose : Int
  , timeUnit : Int
  , strength : Int
  , legato : Int
  -- non-control metadata
  , playedBefore : Array Bool
  , introDone : Bool
  -- control flow
  , currentLine : Int
  , loopCounter : Int
  , returnTo : Int  -- (assuming stack is not needed)
  }

simulateChannel : AsmChannel -> Channel
simulateChannel asmChannel =
  let
    initialState : MachineState
    initialState =
      { octave = 0
      , transpose = 0
      , timeUnit = 1
      , strength = 12
      , legato = 6
      , playedBefore = Array.repeat (List.length asmChannel) False
      , introDone = False
      , currentLine = 0
      , loopCounter = 0
      , returnTo = -1
      }
    emptyChannel : Channel
    emptyChannel = { boot = [], loop = [] }
  in
    simulateWithState (asmChannel, initialState, emptyChannel)

simulateWithState : (AsmChannel, MachineState, Channel) -> Channel
simulateWithState (asmChannel, state, channel) =
  let
    (note, state_, action) = step asmChannel state
  in
    case action of
      Continue -> let
                    channel_ =
                      if not state_.introDone
                      then { channel | boot = maybeSnoc note channel.boot }
                      else { channel | loop = maybeSnoc note channel.loop }
                  in
                    simulateWithState (asmChannel, state_, channel_)
      Halt -> channel

maybeSnoc : Maybe a -> List a -> List a
maybeSnoc ma la = case ma of
  Nothing -> la
  Just a -> la ++ [a]

parseInstruction : String -> (String, List String)
parseInstruction s = s
  |> String.filter (\c -> c /= ',')
  |> words
  |> headtail

type MachineAction
  = Continue
  | Halt

step : AsmChannel -> MachineState -> (Maybe Note, MachineState, MachineAction)
step asm state =
  let
    line = get state.currentLine asm
    incr state_ = {state_ | playedBefore =
                              Array.set state_.currentLine True state_.playedBefore
                          , currentLine = state_.currentLine + 1 }
  in
    if line == "" || startsWith "." line || startsWith "Music_" line
    then if line == ".mainloop:"
         then (Nothing, { state | introDone = True } |> incr, Continue)
         else (Nothing, state |> incr, Continue)
    else
      let
        (root, args) = parseInstruction line
      in
        case root of


        -- control flow
          "sound_call" ->
            let
              label = get 0 args
            in
              (Nothing, state |> jumpCall asm label |> incr, Continue)
          "sound_ret" ->
            if state.returnTo == -1
            then (Nothing, state |> incr, Halt)  -- true termination
            else (Nothing, state |> jumpReturn |> incr, Continue)
          "sound_loop" ->
            let
              times = toInt_ (get 0 args)
              label = get 1 args
            in
              if times == 0
              then (Nothing, incr state, Halt)  -- looping rebranded as 'termination'
              else case state.loopCounter of
                0 -> (Nothing, state |> initializeLoop asm times label |> incr, Continue)
                1 -> (Nothing, { state | loopCounter = 0 } |> incr, Continue)
                _ -> (Nothing, state |> jumpLoop asm label |> incr, Continue)



        -- modifiers
          "note_type" ->
            let
              timeUnit = toInt_ (get 0 args)
              strength = toInt_ (get 1 args)
              legato = toInt_ (get 2 args)
            in
              (Nothing, { state | timeUnit = timeUnit
                                , strength = strength
                                , legato = legato } |> incr, Continue)  -- todo: handle case legato < 0
          "volume_envelope" ->
            let
              strength = toInt_ (get 0 args)
              legato = toInt_ (get 1 args)
            in
              (Nothing, { state | strength = strength
                                , legato = legato } |> incr, Continue)
          "octave" -> (Nothing, {state | octave = toInt_ (get 0 args)} |> incr, Continue)
            -- should one always lower the octave of Ch3 ? this is done later, far up above.
          "transpose" ->
            let
              octaves = toInt_ (get 0 args)
              semitones = toInt_ (get 1 args)
              transpose = 12*octaves + semitones
            in
              (Nothing, {state | transpose = transpose} |> incr, Continue)



        -- actual sounds or silence
          "rest" ->
            let
              z = toInt_ (get 0 args)
              w = state.timeUnit * z
              pb = array_get state.currentLine state.playedBefore
            in (Just { duration = w
                     , what = Nothing
                     , playedBefore = pb }, state |> incr, Continue)
          "note" ->
            let
              x = keyToInt (parseKey (get 0 args))
              { octave, transpose } = state
              y = x + 12*octave + transpose
    
              z = toInt_ (get 1 args)
              w = state.timeUnit * z
              
              defNote = { key = y
                        , strength = state.strength
                        , legato = state.legato
                        }
              pb = array_get state.currentLine state.playedBefore
            in
              (Just { duration = w
                    , what = Just defNote
                    , playedBefore = pb }, state |> incr, Continue)



        -- unused
          "pitch_offset" -> (Nothing, state |> incr, Continue)  -- microtonal?
          "tempo" -> (Nothing, state |> incr, Continue)  -- vertical scale of the sheetmusic?
          "volume" -> (Nothing, state |> incr, Continue)  -- strength, todo
          "duty_cycle" -> (Nothing, state |> incr, Continue)  -- some sort of sound texture
          "vibrato" -> (Nothing, state |> incr, Continue)  -- maybe useful for violin
          "toggle_noise" -> (Nothing, state |> incr, Continue)
          "stereo_panning" -> (Nothing, state |> incr, Continue)


        -- unrecognized
          _ ->  Debug.todo ("unknown command: " ++ root)


jumpCall : AsmChannel -> String -> MachineState -> MachineState
jumpCall asm label state =
  { state | returnTo = state.currentLine
          , currentLine = lookupLabel label asm }

jumpReturn : MachineState -> MachineState
jumpReturn state =
  { state | currentLine = state.returnTo
          , returnTo = -1 }

initializeLoop : AsmChannel -> Int -> String -> MachineState -> MachineState
initializeLoop asm times label state =
  { state | currentLine = lookupLabel label asm
          , loopCounter = times - 1 }

jumpLoop : AsmChannel -> String -> MachineState -> MachineState
jumpLoop asm label state =
  { state | currentLine = lookupLabel label asm
          , loopCounter = state.loopCounter - 1}

lookupLabel : String -> AsmChannel -> Int
lookupLabel label = index (\line -> line == label ++ ":")
