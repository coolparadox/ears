module EarsTypes where

import Data.List
import Text.Pandoc.Builder (Blocks)

class Ears a where
  getEntities :: a -> [Entity]

data Entity = MakeEntity {
  entityLabel :: String,
  entityIsPlural :: Bool,
  entityIsDefined :: Bool,
  entityDescription :: Blocks,
  entityOptionalFeatures :: [String],
  entityBoundaryEvents :: [String],
  entityStates :: [String],
  entityOutcomes :: [String] } deriving (Show, Eq)

instance Ears Entity where
  getEntities entity = [entity]

data Constraint = MakeConstraint {
  constraintEntity :: Entity,
  constraintFeatures :: [String],
  constraintStates :: [String],
  constraintEvents :: [String] } deriving (Show, Eq)

instance Ears Constraint where
  getEntities = getEntities . constraintEntity

data HappyPath = MakeHappyPath {
  happyPathConstraints :: [Constraint],
  happyPathOutcomes :: [String] } deriving (Show, Eq)

instance Ears HappyPath where
  getEntities = nub . concat . (map getEntities) . happyPathConstraints

data UnhappyPath = MakeUnhappyPath {
  unhappyPathTriggeringEvents :: [String],
  unhappyPathMitigationStrategy :: HappyPath } deriving (Show, Eq)

instance Ears UnhappyPath where
  getEntities = getEntities . unhappyPathMitigationStrategy

data Behavior =
  TypicalBehavior HappyPath |
  MitigationBehavior UnhappyPath deriving (Show, Eq)

instance Ears Behavior where
  getEntities (TypicalBehavior happyPath) = getEntities happyPath
  getEntities (MitigationBehavior unhappyPath) = getEntities unhappyPath

data Requirement = MakeRequirement {
  requirementLabel :: String,
  requirementEntity :: Entity,
  requirementBehavior :: Behavior,
  requirementRationale :: String } deriving (Show, Eq)

instance Ears Requirement where
  getEntities requirement = nub (_requirementEntity : _behaviorEntities) where
    _requirementEntity = requirementEntity requirement
    _behaviorEntities = getEntities (requirementBehavior requirement)

data Specification = MakeSpecification {
  specificationSystem :: Entity,
  specificationPurpose :: Blocks,
  specificationScope :: Blocks,
  specificationRequirements :: [Requirement] } deriving (Show, Eq)

instance Ears Specification where
  getEntities specification = nub (_specificationEntity : _requirementsEntities) where
    _specificationEntity = specificationSystem specification
    _requirementsEntities = concat (map getEntities (specificationRequirements specification))

