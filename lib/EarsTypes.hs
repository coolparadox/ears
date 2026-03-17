module EarsTypes where

import EarsUtils (makeTokens, flatify, prependToShall)
import Text.Pandoc.Builder (Blocks)
import Data.List (nub)

-- An entity that may be referenced by requirements.
data Entity = MakeEntity {
  entityName :: String,
  entityIsPlural :: Bool,
  entityIsDefined :: Bool,
  entityDescription :: Blocks } deriving (Show, Eq)

class EarsEntity a where
  -- Get all referenced entities.
  getEntities :: a -> [Entity]

class EarsStatement a where
  -- Get pieces of text that summarize the object.
  getStatement :: a -> [String]

instance EarsEntity Entity where
  getEntities entity = [entity]

instance EarsStatement Entity where
  getStatement (MakeEntity name _ False _) = [name]
  getStatement (MakeEntity name _ True _) = ["the", name]

data Constraint = MakeConstraint {
  constraintEntity :: Entity,
  constraintFeatures :: [String],
  constraintStates :: [String],
  constraintEvents :: [String] } deriving (Show, Eq)

instance EarsEntity Constraint where
  getEntities = getEntities . constraintEntity

instance EarsStatement Constraint where
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

instance EarsEntity HappyPath where
  getEntities = nub . concat . (map getEntities) . happyPathConstraints

instance EarsStatement HappyPath where
  getStatement (MakeHappyPath constraints outcomes) = _constraintsTokens ++ _outcomesTokens where
    _constraintsTokens = concat (map getStatement constraints)
    _outcomesTokens = makeTokens ["shall"] outcomes

data UnhappyPath = MakeUnhappyPath {
  unhappyPathTriggeringEvents :: [String],
  unhappyPathMitigationStrategy :: HappyPath } deriving (Show, Eq)

instance EarsEntity UnhappyPath where
  getEntities = getEntities . unhappyPathMitigationStrategy

instance EarsStatement UnhappyPath where
  getStatement (MakeUnhappyPath triggers mitigation) = prependToShall (_triggersStatement ++ ["then"]) _mitigationStatement where
    _triggersStatement = makeTokens ["if"] triggers
    _mitigationStatement = getStatement mitigation

data Behavior =
  TypicalBehavior HappyPath |
  MitigationBehavior UnhappyPath deriving (Show, Eq)

instance EarsEntity Behavior where
  getEntities (TypicalBehavior happyPath) = getEntities happyPath
  getEntities (MitigationBehavior unhappyPath) = getEntities unhappyPath

instance EarsStatement Behavior where
  getStatement (TypicalBehavior happyPath) = getStatement happyPath
  getStatement (MitigationBehavior unhappyPath) = getStatement unhappyPath

data Requirement = MakeRequirement {
  requirementLabel :: String,
  requirementEntity :: Entity,
  requirementBehavior :: Behavior,
  requirementRationale :: String } deriving (Show, Eq)

instance EarsEntity Requirement where
  getEntities requirement = nub (_requirementEntity : _behaviorEntities) where
    _requirementEntity = requirementEntity requirement
    _behaviorEntities = getEntities (requirementBehavior requirement)

instance EarsStatement Requirement where
  getStatement (MakeRequirement _ entity behavior _) = prependToShall _systemTokens _behaviorTokens where
    _systemTokens = getStatement entity
    _behaviorTokens = getStatement behavior

data RequirementGroup = MakeRequirementGroup {
  requirementGroupTitle :: String,
  requirementGroupChildren :: [Either Requirement RequirementGroup] } deriving (Show, Eq)

instance EarsEntity RequirementGroup where
  getEntities (MakeRequirementGroup _ []) = []
  getEntities (MakeRequirementGroup _ (req:reqs)) = nub (_headRequirements ++ _tailRequirements) where
    _headRequirements = either getEntities getEntities req
    _tailRequirements = concat (map (either getEntities getEntities) reqs)

instance EarsStatement RequirementGroup where
  getStatement _ = []

data Specification = MakeSpecification {
  specificationSystem :: Entity,
  specificationPurpose :: Blocks,
  specificationScope :: Blocks,
  specificationEntities :: [Entity],
  specificationRequirements :: [Either Requirement RequirementGroup] } deriving (Show, Eq)

instance EarsEntity Specification where
  getEntities (MakeSpecification system _ _ entities requirements) = nub ([system] ++ entities ++ _requirementsEntities) where
    _requirementsEntities = concat (map (either getEntities getEntities) requirements)

instance EarsStatement Specification where
  getStatement specification = _systemTokens ++ _titleTokens where
    _systemTokens = getStatement (specificationSystem specification)
    _titleTokens = ["Software", "Requirement", "Specification"]

