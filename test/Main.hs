module Main (main) where

import EarsTypes

import Data.Text (pack)
import Text.Pandoc.Definition (Block(..), Inline(..))

entity1 :: Entity
entity1 = MakeEntity {
  entityLabel = "SystemXWZ",
  entityIsPlural = False,
  entityIsDefined = False,
  entityDescription = Para [ Str (pack "The first entity.") ],
  entityOptionalFeatures = [],
  entityBoundaryEvents = [],
  entityStates = [],
  entityOutcomes = [] }

happyPath1 :: HappyPath
happyPath1 = MakeHappyPath {
  happyPathConstraints = [],
  happyPathOutcomes = [ "works" ] }

requirement1 :: Requirement
requirement1 = MakeRequirement {
  requirementLabel = "REQ-01",
  requirementEntity = entity1,
  requirementBehavior = TypicalBehavior happyPath1,
  requirementRationale = "The system must attend to expectations."
}

srs :: Specification
srs = MakeSpecification {
  specificationRequirements = [requirement1] }

main :: IO ()
main = do
  print srs

