module Protocol exposing (..)

import String exposing (indexes,length,slice,startsWith,toInt)
import List exposing (head,sum,map,drop,filter,indexedMap,repeat)
import Array exposing (Array)

type alias Metadata =
  { title : String
  , covers : List String
  , songs : List {name : String, thumb : String, addr : String}
  }

type alias Song =
  { name : String
  , channels : List Channel
  , measures : List Int
  }

type alias Channel =
  { boot : List Note
  , loop : List Note
  }

type alias Note =
  { duration : Int
  , what : Maybe DefiniteNote
  , playedBefore : Bool
  }
  
type alias DefiniteNote =
  { key : Int       -- typically between 0 and 100
  , legato : Int    -- <= 4 (esp. 1 or 2) means staccato; >= 7 means legato
  , strength : Int  -- typically between 2 and 13, with 6 being pretty silent and 10 being an accent
  }
  
type Key = C0 | C1 | D0 | D1 | E0 | F0 | F1 | G0 | G1 | A0 | A1 | B0

totalDuration : List Channel -> (Int, Int)
totalDuration channels =
  let
    xs = totalDurations channels
    ys = repeat (List.length xs) (get 0 xs)
  in
    if xs == ys
    then get 0 xs
--    else Debug.todo ("the durations don't match! " ++ Debug.toString xs)
    else Debug.log (Debug.toString xs) (get 0 xs)

totalDurations : List Channel -> (List (Int, Int))
totalDurations channels =
  let
    duration_ { duration , what } = duration
    channelToIntInt { boot, loop } = (sum (map duration_ boot), sum (map duration_ loop))
  in
    map channelToIntInt channels

transpose : Int -> Channel -> Channel
transpose offset channel =
  let
    { boot, loop } = channel
  in
    { channel
    | boot = map (transposeNote offset) boot
    , loop = map (transposeNote offset) loop
    }

transposeNote : Int -> Note -> Note
transposeNote offset note =
  { note | what = note.what |> Maybe.map (transposeDefiniteNote offset) }

transposeDefiniteNote : Int -> DefiniteNote -> DefiniteNote
transposeDefiniteNote offset defNote =
  { defNote | key = defNote.key + offset }

parseKey : String -> Key
parseKey s = case s of
  "C_" -> C0
  "C#" -> C1
  "D_" -> D0
  "D#" -> D1
  "E_" -> E0
  "F_" -> F0
  "F#" -> F1
  "G_" -> G0
  "G#" -> G1
  "A_" -> A0
  "A#" -> A1
  "B_" -> B0
  _ -> Debug.todo ("invalid key " ++ s)

keyToInt : Key -> Int
keyToInt k = case k of
  C0 -> 0
  C1 -> 1
  D0 -> 2
  D1 -> 3
  E0 -> 4
  F0 -> 5
  F1 -> 6
  G0 -> 7
  G1 -> 8
  A0 -> 9
  A1 -> 10
  B0 -> 11

intToKey : Int -> Key
intToKey k = case modBy 12 k of
  0 -> C0
  1 -> C1
  2 -> D0
  3 -> D1
  4 -> E0
  5 -> F0
  6 -> F1
  7 -> G0
  8 -> G1
  9 -> A0
  10 -> A1
  11 -> B0
  _ -> Debug.todo "mod error"

isBlack : Key -> Bool
isBlack k = case k of
  C1 -> True
  D1 -> True
  F1 -> True
  G1 -> True
  A1 -> True
  _ -> False

removeHeader : String -> String  -- workaround for elm-reactor
removeHeader s = if (startsWith "<!DOCTYPE HTML>" s)
                 then extractBetween "<pre><code>" s "</code></pre>"
                 else s

extractBetween : String -> String -> String -> String
extractBetween s0 s s1 =
  case (head (indexes s0 s), head (indexes s1 s)) of
    (Just i0, Just i1) -> slice (i0 + length s0) i1 s
    (_, _) -> Debug.todo "header format error"









toInt_ : String -> Int
toInt_ s = case toInt s of
  Just i -> i
  Nothing -> Debug.todo "invalid integer"

index : (a -> Bool) -> List a -> Int
index f xs = case filter (\(i, a) -> f a) (indexedMap Tuple.pair xs) of
  (i, x) :: _ -> i
  [] -> Debug.todo "index not found"

listIndexes : (a -> Bool) -> List a -> List Int
listIndexes f xs =
  xs
  |> indexedMap Tuple.pair
  |> filter (\(i, a) -> f a)
  |> map Tuple.first

headtail : List a -> (a, List a)
headtail la = case la of
  [] -> Debug.todo "list is empty"
  (a :: la_) -> (a, la_)

get : Int -> List a -> a
get n la =
  let
    (head, tail) = headtail (drop n la)
  in head

array_get : Int -> Array a -> a
array_get n la =
  case Array.get n la of
    Nothing -> Debug.todo "array out of bounds"
    Just a -> a

cumsum : List Int -> List Int
cumsum xs =
  let
    helper xs_ u =
      case xs_ of
        [] -> u :: []
        (x :: xs__) -> u :: helper xs__ (x + u)
  in
    helper xs 0
