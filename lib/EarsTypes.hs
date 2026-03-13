module EarsTypes where

import EarsUtils (makeTokens, flatify, prependToShall)

import Text.Pandoc.Builder (Blocks)

import Data.List (nub)

class Ears a where
  getEntities :: a -> [Entity]
  getStatement :: a -> [String]

data Entity = MakeEntity {
  entityName :: String,
  entityIsPlural :: Bool,
  entityIsDefined :: Bool,
  entityDescription :: Blocks } deriving (Show, Eq)

instance Ears Entity where
  getEntities entity = [entity]
  getStatement (MakeEntity name _ False _) = [name]
  getStatement (MakeEntity name _ True _) = ["the", name]

data Constraint = MakeConstraint {
  constraintEntity :: Entity,
  constraintFeatures :: [String],
  constraintStates :: [String],
  constraintEvents :: [String] } deriving (Show, Eq)

instance Ears Constraint where
  getEntities = getEntities . constraintEntity
  getStatement (MakeConstraint entity features states events) = flatify [_featuresStatement, _statesStatement, _eventsStatement] where
    _featuresStatement = makeTokens ("where" : _entityHasStatement) features
    _statesStatement = makeTokens ("while" : _entityIsStatement) states
    _eventsStatement = makeTokens ("when" : _entityIsStatement) events
    _entityHasStatement = _entityStatement ++ if entityIsPlural entity then ["have"] else ["has"]
    _entityIsStatement = _entityStatement ++ if entityIsPlural entity then ["are"] else ["is"]
    _entityStatement = getStatement entity

data HappyPath = MakeHappyPath {
  happyPathConstraints :: [Constraint],
  happyPathOutcomes :: [String] } deriving (Show, Eq)

instance Ears HappyPath where
  getEntities = nub . concat . (map getEntities) . happyPathConstraints
  getStatement (MakeHappyPath constraints outcomes) = _constraintsTokens ++ _outcomesTokens where
    _constraintsTokens = concat (map getStatement constraints)
    _outcomesTokens = makeTokens ["shall"] outcomes

data UnhappyPath = MakeUnhappyPath {
  unhappyPathTriggeringEvents :: [String],
  unhappyPathMitigationStrategy :: HappyPath } deriving (Show, Eq)

instance Ears UnhappyPath where
  getEntities = getEntities . unhappyPathMitigationStrategy
  getStatement (MakeUnhappyPath triggers mitigation) = prependToShall (_triggersStatement ++ ["then"]) _mitigationStatement where
    _triggersStatement = makeTokens ["if"] triggers
    _mitigationStatement = getStatement mitigation

data Behavior =
  TypicalBehavior HappyPath |
  MitigationBehavior UnhappyPath deriving (Show, Eq)

instance Ears Behavior where
  getEntities (TypicalBehavior happyPath) = getEntities happyPath
  getEntities (MitigationBehavior unhappyPath) = getEntities unhappyPath
  getStatement (TypicalBehavior happyPath) = getStatement happyPath
  getStatement (MitigationBehavior unhappyPath) = getStatement unhappyPath

data Requirement = MakeRequirement {
  requirementLabel :: String,
  requirementEntity :: Entity,
  requirementBehavior :: Behavior,
  requirementRationale :: String } deriving (Show, Eq)

instance Ears Requirement where
  getEntities requirement = nub (_requirementEntity : _behaviorEntities) where
    _requirementEntity = requirementEntity requirement
    _behaviorEntities = getEntities (requirementBehavior requirement)
  getStatement (MakeRequirement _ entity behavior _) = prependToShall _systemTokens _behaviorTokens where
    _systemTokens = getStatement entity
    _behaviorTokens = getStatement behavior

data Specification = MakeSpecification {
  specificationSystem :: Entity,
  specificationPurpose :: Blocks,
  specificationScope :: Blocks,
  specificationRequirements :: [Requirement] } deriving (Show, Eq)

instance Ears Specification where
  getEntities specification = nub (_specificationEntity : _requirementsEntities) where
    _specificationEntity = specificationSystem specification
    _requirementsEntities = concat (map getEntities (specificationRequirements specification))
  getStatement specification = _systemTokens ++ _titleTokens where
    _systemTokens = getStatement (specificationSystem specification)
    _titleTokens = ["Software", "Requirement", "Specification"]

