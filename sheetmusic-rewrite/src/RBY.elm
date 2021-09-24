module RBY exposing (..)

-- Load all RBY song metadata.
--

import Protocol exposing (Metadata)

parseMetadata : String -> Metadata
parseMetadata fullText =
  { title = "Pokémon Red, Blue, Yellow"
  , covers = []
  , songs = []
  }

