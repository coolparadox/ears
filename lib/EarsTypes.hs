module EarsTypes where

import EarsUtils (joinWords, makeStatement, andify)

import Text.Pandoc.Builder (Blocks)

import Data.List (nub, intersperse)

class Ears a where
  getEntities :: a -> [Entity]
  getStatement :: a -> Bool -> String

data Entity = MakeEntity {
  entityName :: String,
  entityIsPlural :: Bool,
  entityIsDefined :: Bool,
  entityDescription :: Blocks,
  entityOptionalFeatures :: [String],
  entityBoundaryEvents :: [String],
  entityStates :: [String],
  entityOutcomes :: [String] } deriving (Show, Eq)

instance Ears Entity where
  getEntities entity = [entity]
  getStatement (MakeEntity name _ False _ _ _ _ _) _ = name
  getStatement (MakeEntity name _ True _ _ _ _ _) isStart = makeStatement isStart ["the", name]

data Constraint = MakeConstraint {
  constraintEntity :: Entity,
  constraintFeatures :: [String],
  constraintStates :: [String],
  constraintEvents :: [String] } deriving (Show, Eq)

instance Ears Constraint where
  getEntities = getEntities . constraintEntity
  getStatement constraint _ = "FIXME: Constraint statement"

data HappyPath = MakeHappyPath {
  happyPathConstraints :: [Constraint],
  happyPathOutcomes :: [String] } deriving (Show, Eq)

instance Ears HappyPath where
  getEntities = nub . concat . (map getEntities) . happyPathConstraints
  getStatement (MakeHappyPath _ outcomes) isStart = makeStatement isStart (andify outcomes)

data UnhappyPath = MakeUnhappyPath {
  unhappyPathTriggeringEvents :: [String],
  unhappyPathMitigationStrategy :: HappyPath } deriving (Show, Eq)

instance Ears UnhappyPath where
  getEntities = getEntities . unhappyPathMitigationStrategy
  getStatement unhappyPath _ = "FIXME"

data Behavior =
  TypicalBehavior HappyPath |
  MitigationBehavior UnhappyPath deriving (Show, Eq)

instance Ears Behavior where
  getEntities (TypicalBehavior happyPath) = getEntities happyPath
  getEntities (MitigationBehavior unhappyPath) = getEntities unhappyPath
  getStatement (TypicalBehavior happyPath) isStart = getStatement happyPath isStart

data Requirement = MakeRequirement {
  requirementLabel :: String,
  requirementEntity :: Entity,
  requirementBehavior :: Behavior,
  requirementRationale :: String } deriving (Show, Eq)

instance Ears Requirement where
  getEntities requirement = nub (_requirementEntity : _behaviorEntities) where
    _requirementEntity = requirementEntity requirement
    _behaviorEntities = getEntities (requirementBehavior requirement)
  getStatement (MakeRequirement _ entity behavior _) isStart =
    joinWords [_system, "shall", _response] where
    _system = getStatement entity isStart
    _response = getStatement behavior False

data Specification = MakeSpecification {
  specificationSystem :: Entity,
  specificationPurpose :: Blocks,
  specificationScope :: Blocks,
  specificationRequirements :: [Requirement] } deriving (Show, Eq)

instance Ears Specification where
  getEntities specification = nub (_specificationEntity : _requirementsEntities) where
    _specificationEntity = specificationSystem specification
    _requirementsEntities = concat (map getEntities (specificationRequirements specification))
  getStatement specification _ = "FIXME"

