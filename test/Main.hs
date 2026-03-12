module Main (main) where

import EarsTypes
import EarsDoc

import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Writers
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Error

entity1 :: Entity
entity1 = MakeEntity {
  entityLabel = "SystemXWZ",
  entityIsPlural = False,
  entityIsDefined = False,
  entityDescription = Para [ Str (T.pack "The first entity.") ],
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

outDoc :: Pandoc
outDoc = toDoc srs

main :: IO ()
main = do
  outContent <- runIO (writeAsciiDoc def outDoc) >>= handleError
  print outContent

