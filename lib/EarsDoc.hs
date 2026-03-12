module EarsDoc where

import EarsTypes

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

toDoc :: Specification -> Pandoc
toDoc srs = doc $ header 1 (text (T.pack "Specification"))


