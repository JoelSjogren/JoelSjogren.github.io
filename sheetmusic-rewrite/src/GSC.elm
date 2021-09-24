module GSC exposing (..)

-- Load all GSC song metadata.
--

import String exposing (startsWith,slice,lines,dropLeft,endsWith,contains,toInt,words)
import Html exposing (Html, text, pre, h1, h2, div, img, hr)
import Html.Attributes exposing (href, src, width)
import List exposing (map,filter,indexedMap,drop,partition,take,drop,isEmpty,append,concat,repeat)
import Tuple exposing (first,second)

import Protocol exposing (Metadata,Song,Channel,Note,Key,keyToInt,parseKey,get,toInt_,headtail,index,transpose)

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
  { name = slice 12 -4 path
  , thumb = "../data/gsc/thumb/" ++ slice 12 -4 path ++ ".png"
  , addr = "../data/gsc/" ++ path
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

type alias AsmBlock = List String

type alias StructuredAsmChannel =
  { init : List AsmBlock
  , loops : List AsmBlock  -- beware bad name: init may also contain loops
  , subs : List AsmBlock
  }

type alias InlinedAsmChannel =
  { boot : AsmBlock
  , loop : AsmBlock
  }

loadSong : { name : String, asm : String } -> Song
loadSong { name, asm } =
  let
    asmLines = lines asm |> filter (not << String.isEmpty)

    channels =
      asmLines
      |> listIndexes (\line -> startsWith "Music_" line && contains "_Ch" line && endsWith ":" line)
      |> intoBlocks asmLines
      |> map simulateChannel
      |> lowerCh3  -- (why?)
  in
    { name = name
    , channels = channels
    }

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

simulateChannel : AsmBlock -> Channel
simulateChannel asmLines =
  let
    { boot, loop } =
      asmLines
      |> listIndexes (\line -> not (startsWith "\t" line))
      |> intoBlocks asmLines
      |> makeStructuredAsm
      |> makeInlinedAsm

    initialState : MachineState
    initialState =
      { octave = 0
      , transpose = 0
      , timeUnit = 1
      , strength = 12
      , legato = 6
      }
    
    (boot_, newState) = boot |> executeBlock initialState
    (loop_, endState) = loop |> executeBlock newState
  in { boot = boot_, loop = loop_ }

makeStructuredAsm : List AsmBlock -> StructuredAsmChannel
makeStructuredAsm blocks =
  let
    labelContains s = (\block -> contains s (get 0 block))
    i = index (labelContains "mainloop") blocks
    (init, rest) = (take i blocks, drop i blocks)
    (loops, subs) = partition (labelContains "loop") rest
  in { init = init, loops = loops, subs = subs }
  

makeInlinedAsm : StructuredAsmChannel -> InlinedAsmChannel
makeInlinedAsm { init, loops, subs } =
{-  let
    boot = init
    -- first inline .loopx, then inline .subx
    loop_ = concat (map expand loops)
    loop = inline loop_ subs
  in { boot = boot, loop = loop }
-}
  let
    f u = inline (concat (map expand u)) subs
  in { boot = f init, loop = f loops }

expand : AsmBlock -> AsmBlock  -- playedBefore would be initialized here, once implemented
expand block =
    case listIndexes (contains "sound_loop") block of
      [] -> block
      i :: _ ->
        let
          (root, args) =
            block
            |> get i
            |> parseInstruction
          j = toInt_ (get 0 args)
        in
          if (j == 0)
          then block
          else append (concat (repeat j (take i block))) (drop i block)
--      u -> Debug.todo ("expand bug - too many sound loops " ++ String.fromInt (List.length u) ++ (Debug.toString block))

inline : AsmBlock -> List AsmBlock -> AsmBlock
inline block subs = case block of
  x :: xs ->
    let
      (root, args) = parseInstruction x
    in
      if (root /= "sound_call")
      then x :: inline xs subs
      else let
             l = String.length x
             i = toInt_ (slice (l-1) l x)
           in
             append (get (i - 1) subs) (inline xs subs)
  [] -> []

{-
type alias AsmBlock = List String

type alias StructuredAsmChannel =
  { init : AsmBlock
  , loops : List AsmBlock
  , subs : List AsmBlock
  }

type alias InlinedAsmChannel =
  { boot : AsmBlock
  , loop : AsmBlock
  }
-}

type alias MachineState =
  { octave : Int
  , transpose : Int
  , timeUnit : Int
  , strength : Int
  , legato : Int
  }

executeBlock : MachineState -> AsmBlock -> (List Note, MachineState)
executeBlock state block = case block of
  [] -> ([], state)
  (inst :: block_) ->
    let
      (note, state_) = interpret (inst, state)
      (notes, finalState) = executeBlock state_ block_
    in (maybeAppend note notes, finalState)

maybeAppend : Maybe a -> List a -> List a
maybeAppend ma la = case ma of
  Nothing -> la
  Just a -> a :: la

parseInstruction : String -> (String, List String)
parseInstruction s = s
  |> String.filter (\c -> c /= ',')
  |> words
  |> headtail

--type alias Instruction =
--  { root : String
--  , args : List String
--  , playedBefore : Bool
--  }

interpret : (String, MachineState) -> (Maybe Note, MachineState)
interpret (inst, state) =
  let
    (root, args) = parseInstruction inst
  in
    case root of
      "rest" ->
        let
          z = toInt_ (get 0 args)
          w = state.timeUnit * z
        in (Just { duration = w, what = Nothing }, state)
      "octave" -> (Nothing, {state | octave = toInt_ (get 0 args)})
        -- should one always lower the octave of Ch3 ? this is done later, far up above
      "transpose" -> (Nothing, {state | transpose = 12*toInt_ (get 0 args) + toInt_ (get 1 args)})
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
        in
          (Just { duration = w, what = Just defNote }, state)
      "note_type" ->
        let
          timeUnit = toInt_ (get 0 args)
          strength = toInt_ (get 1 args)
          legato = toInt_ (get 2 args)
        in
          (Nothing, { state | timeUnit = timeUnit
                            , strength = strength
                            , legato = legato })  -- todo: handle case legato < 0
      "volume_envelope" ->
        let
          strength = toInt_ (get 0 args)
          legato = toInt_ (get 1 args)
        in
          (Nothing, { state | strength = strength
                            , legato = legato })
         
      -- the following instructions are a bit unclear
      --"pitch_offset" -> (Nothing, {state | transpose = toInt_ (get 0 args)})
      -- microtonal ?
      "pitch_offset" -> (Nothing, state)
      -- the following instructions are ignored
      "tempo" -> (Nothing, state)  -- might be used to set the vertical scale of the sheetmusic
      "volume" -> (Nothing, state)
      "duty_cycle" -> (Nothing, state)
      "vibrato" -> (Nothing, state)
      -- the following are handled elsewhere
      "sound_call" -> (Nothing, state)
      "sound_loop" -> (Nothing, state)
      "sound_ret" -> (Nothing, state)
      _ -> if startsWith "Music_" root
           || startsWith ".mainloop" root
           || startsWith ".loop" root
           || startsWith ".sub" root
           then (Nothing, state)
           -- all other instructions are unknown
           else Debug.todo ("unknown command: " ++ root)






