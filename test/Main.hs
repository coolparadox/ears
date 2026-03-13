{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import EarsTypes
import EarsDoc

import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Writers
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Error
import System.IO

entity1 :: Entity
entity1 = MakeEntity {
  entityName = "SystemXWZ",
  entityIsPlural = False,
  entityIsDefined = False,
  entityDescription = para "The first entity." }

entity2 :: Entity
entity2 = MakeEntity {
  entityName = "aircrafts",
  entityIsPlural = True,
  entityIsDefined = True,
  entityDescription = para "Travelling or grounded flying devices." }

happyPath1 :: HappyPath
happyPath1 = MakeHappyPath {
  happyPathConstraints = [ MakeConstraint entity1 ["overspeed protection", "wings"] [] ["turned off"], MakeConstraint entity2 [] ["on the ground"] [] ],
  happyPathOutcomes = [ "silentiate", "bring peace" ] }

requirement1 :: Requirement
requirement1 = MakeRequirement {
  requirementLabel = "REQ-01",
  requirementEntity = entity1,
  requirementBehavior = TypicalBehavior happyPath1,
  requirementRationale = "The system must attend to expectations." }

unhappyPath1 :: UnhappyPath
unhappyPath1 = MakeUnhappyPath {
  unhappyPathTriggeringEvents = ["it rains"],
  unhappyPathMitigationStrategy = MakeHappyPath {
    happyPathConstraints = [ MakeConstraint entity1 ["windows"] [] [] ],
    happyPathOutcomes = ["close its windows"] } }

requirement2 :: Requirement
requirement2 = MakeRequirement {
  requirementLabel = "REQ-02",
  requirementEntity = entity1,
  requirementBehavior = MitigationBehavior unhappyPath1,
  requirementRationale = "The system must avoid bad behavior."
}

purpose :: Blocks
purpose = para "The System is an internally developed proprietary software."

scope :: Blocks
scope = para "This document contains software requirements for The System software release."

srs :: Specification
srs = MakeSpecification {
  specificationSystem = entity1,
  specificationPurpose = purpose,
  specificationScope = scope,
  specificationRequirements = [requirement1, requirement2] }

document :: Pandoc
document = weave srs

main :: IO ()
main = do
  outHandle <- openFile "srs.md" WriteMode
  content <- runIO (writeMarkdown def document) >>= handleError
  hPutStr outHandle (T.unpack content)
  hClose outHandle
  putStrLn "Created srs.md"

