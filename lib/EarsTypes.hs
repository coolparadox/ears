module EarsTypes where

import Text.Pandoc.Builder (Blocks)

data Entity = MakeEntity {
  entityLabel :: String,
  entityIsPlural :: Bool,
  entityIsDefined :: Bool,
  entityDescription :: Blocks,
  entityOptionalFeatures :: [String],
  entityBoundaryEvents :: [String],
  entityStates :: [String],
  entityOutcomes :: [String] } deriving Show

data Constraint = MakeConstraint {
  constraintEntity :: Entity,
  constraintFeatures :: [String],
  constraintStates :: [String],
  constraintEvents :: [String] } deriving Show

data HappyPath = MakeHappyPath {
  happyPathConstraints :: [Constraint],
  happyPathOutcomes :: [String] } deriving (Show)

data UnhappyPath = MakeUnhappyPath {
  unhappyPathTriggeringEvents :: [String],
  unhappyPathMitigationStrategy :: HappyPath } deriving Show

data Behavior = TypicalBehavior HappyPath | MitigationBehavior UnhappyPath deriving Show

data Requirement = MakeRequirement {
  requirementLabel :: String,
  requirementEntity :: Entity,
  requirementBehavior :: Behavior,
  requirementRationale :: String } deriving Show

data Specification = MakeSpecification {
  specificationSystem :: Entity,
  specificationPurpose :: Blocks,
  specificationScope :: Blocks,
  specificationRequirements :: [Requirement] } deriving (Show)

