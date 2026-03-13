module EarsDoc where

import EarsTypes
import EarsUtils (joinWords)

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

class (Ears a) => EarsDoc a where
  makeStatement :: Bool -> a -> String
  makeStatement isStart x = joinWords isStart (getStatement x)
  toBlocks :: a -> Blocks

instance EarsDoc Entity where
  toBlocks entity = _header <> _body where
    _header = header 2 (text (T.pack (makeStatement True entity)))
    _body = entityDescription entity

instance EarsDoc Requirement where
  toBlocks requirement = _header <> _body where
    _header = header 2 (text (T.pack (requirementLabel requirement)))
    _body = para (text (T.pack (makeStatement True requirement ++ "."))) <>
      para (text (T.pack ("Rationale: " ++ requirementRationale requirement)))

instance EarsDoc Specification where
  toBlocks specification = _purpose <> _scope <> _definitions <> _requirements where
    _purpose = header 1 (text (T.pack "Purpose")) <> (specificationPurpose specification)
    _scope = header 1 (text (T.pack "Scope")) <> (specificationScope specification)
    _definitions = header 1 (text (T.pack "Definitions")) <> _definitionsBody
    _definitionsBody = fromList (concat (map toList (map toBlocks (getEntities specification))))
    _requirements = header 1 (text (T.pack "Requirements")) <> _requirementsBody
    _requirementsBody = fromList (concat (map toList (map toBlocks (specificationRequirements specification))))

weave :: Specification -> Pandoc
weave specification = setTitle (text (T.pack _docTitle)) $ doc $ _body where
  _docTitle = makeStatement True specification
  _body = toBlocks specification


