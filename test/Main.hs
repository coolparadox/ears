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
  entityLabel = "SystemXWZ",
  entityIsPlural = False,
  entityIsDefined = False,
  entityDescription = para "The first entity.",
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

purpose :: Blocks
purpose = para "The System is an internally developed proprietary software."

scope :: Blocks
scope = para "This document contains software requirements for The System software release."

srs :: Specification
srs = MakeSpecification {
  specificationSystem = entity1,
  specificationPurpose = purpose,
  specificationScope = scope,
  specificationRequirements = [requirement1] }

document :: Pandoc
document = weave srs

main :: IO ()
main = do
  outHandle <- openFile "srs.md" WriteMode
  content <- runIO (writeMarkdown def document) >>= handleError
  --print content
  hPutStr outHandle (T.unpack content)
  hClose outHandle
  putStrLn "Created srs.md"

