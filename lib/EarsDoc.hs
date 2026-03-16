module EarsDoc where

import EarsTypes
import EarsUtils (joinWords, flatifyBlocks)

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

class (EarsStatement a) => EarsDoc a where
  makeStatement :: Bool -> a -> String
  makeStatement isStartOfSentence x = joinWords isStartOfSentence (getStatement x)
  toBlocks :: Int -> a -> Blocks

instance EarsDoc Entity where
  toBlocks headerLevel entity = _header <> _body where
    _header = header headerLevel (text (T.pack (makeStatement True entity)))
    _body = entityDescription entity

instance EarsDoc Requirement where
  toBlocks _ requirement = _statement <> _rationale where
    _statement = para (_label <> _separator <> _text)
    _label = strong (text (T.pack (requirementLabel requirement)))
    _separator = text (T.pack " - ")
    _text = emph (text (T.pack ((makeStatement True requirement) ++ ".")))
    _rationale = para (text (T.pack ("(Rationale: " ++ requirementRationale requirement ++ ")")))

instance EarsDoc RequirementGroup where
  toBlocks headerLevel (MakeRequirementGroup title reqs) = _header <> _body where
    _header = header headerLevel (text (T.pack (title)))
    _body = flatifyBlocks (map (either (toBlocks (headerLevel+1)) (toBlocks (headerLevel+1))) reqs)

instance EarsDoc Specification where
  toBlocks headerLevel specification = _purpose <> _scope <> _definitions <> _requirements where
    _purpose = header headerLevel (text (T.pack "Purpose")) <> (specificationPurpose specification)
    _scope = header headerLevel (text (T.pack "Scope")) <> (specificationScope specification)
    _definitions = header headerLevel (text (T.pack "Definitions")) <> _definitionsBody
    _definitionsBody = flatifyBlocks (map (toBlocks (headerLevel + 1)) (getEntities specification))
    _requirements = header headerLevel (text (T.pack "Requirements")) <> _requirementsBody
    _requirementsBody = flatifyBlocks (map (either (toBlocks (headerLevel + 1)) (toBlocks (headerLevel + 1))) (specificationRequirements specification))

weave :: Specification -> Pandoc
weave specification = setTitle (text (T.pack _docTitle)) $ doc $ _body where
  _docTitle = makeStatement True specification
  _body = toBlocks 1 specification


