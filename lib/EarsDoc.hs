module EarsDoc where

import EarsTypes

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

toDoc :: Specification -> Pandoc
toDoc srs = setTitle (text (T.pack _docTitle)) $ doc $
  header 1 (text (T.pack "Purpose")) <> (specificationPurpose srs) <>
  header 1 (text (T.pack "Scope")) <> (specificationScope srs)
  where
    _docTitle = _systemLabel ++ " Software Requirement Specification"
    _systemLabel = entityLabel _system
    _system = specificationSystem srs

