module Utils.Containers where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

mapToSeq :: HashMap k v -> Seq (k, v)
mapToSeq = Seq.fromList . Map.toList
