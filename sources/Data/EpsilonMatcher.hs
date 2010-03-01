-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1416:@thin EpsilonMatcher.hs
-- @@language Haskell

module Data.EpsilonMatcher where

-- @<< Import needed modules >>
-- @+node:gcross.20100228202857.1293:<< Import needed modules >>
import Control.Arrow
import Control.Monad.State.Strict

import qualified Data.COrdering as COrdering
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree.AVL (AVL)
import qualified Data.Tree.AVL as AVL
-- @-node:gcross.20100228202857.1293:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100228202857.1294:Types
-- @+node:gcross.20100228202857.1296:Match
data Match valueType = Match
    {   matchValue :: valueType
    ,   matchKey :: Int
    }
-- @-node:gcross.20100228202857.1296:Match
-- @+node:gcross.20100228202857.1295:EpsilonMatcher
data EpsilonMatcher valueType = EpsilonMatcher
    {   epsilonMatcherTree :: AVL (Match valueType)
    ,   epsilonMatcherNextIndex :: Int
    ,   epsilonMatcherTolerance :: valueType
    }
-- @-node:gcross.20100228202857.1295:EpsilonMatcher
-- @+node:gcross.20100228202857.1309:EpsilonMatcherState
type EpsilonMatcherState valueType resultType = State (EpsilonMatcher valueType) resultType
-- @-node:gcross.20100228202857.1309:EpsilonMatcherState
-- @-node:gcross.20100228202857.1294:Types
-- @+node:gcross.20100228202857.1297:Functions
-- @+node:gcross.20100228202857.1301:newEpsilonMatcher
newEpsilonMatcher :: valueType -> EpsilonMatcher valueType
newEpsilonMatcher tolerance = EpsilonMatcher AVL.empty 0 tolerance
-- @-node:gcross.20100228202857.1301:newEpsilonMatcher
-- @+node:gcross.20100228202857.1303:getMatchMap
getMatchMap :: EpsilonMatcher valueType -> Map Int Int
getMatchMap =
    Map.fromList
    .
    flip zip [0..]
    .
    map matchKey
    .
    AVL.asListL
    .
    epsilonMatcherTree
-- @-node:gcross.20100228202857.1303:getMatchMap
-- @+node:gcross.20100228202857.1298:match
match ::
    (Ord valueType, Num valueType) =>
    valueType ->
    EpsilonMatcher valueType ->
    (Int,EpsilonMatcher valueType)
match lookup_value matcher@(EpsilonMatcher match_tree next_index tolerance) =
    case AVL.tryRead match_tree comparer of
        Just match_key -> (match_key,matcher)
        Nothing ->
            (next_index
            ,EpsilonMatcher
                (AVL.push comparer2 (Match lookup_value next_index) match_tree)
                (next_index+1)
                tolerance
            )
  where
    comparer match@(Match match_value match_key) =
        if abs (lookup_value-match_value) < tolerance
            then COrdering.Eq match_key
            else comparer2 match
    comparer2 (Match match_value _) =
        if lookup_value < match_value
            then COrdering.Lt
            else COrdering.Gt
-- @-node:gcross.20100228202857.1298:match
-- @+node:gcross.20100228202857.1299:lookupMatch
lookupMatch ::
    (Ord valueType, Num valueType) =>
    valueType ->
    EpsilonMatcherState valueType Int
lookupMatch = State . match
-- @-node:gcross.20100228202857.1299:lookupMatch
-- @+node:gcross.20100228202857.1308:runEpsilonMatcher
runEpsilonMatcher ::
    valueType ->
    EpsilonMatcherState valueType resultType
    -> (resultType, Map Int Int)
runEpsilonMatcher tolerance stateRunner =
    second getMatchMap
    .
    runState stateRunner
    .
    newEpsilonMatcher
    $
    tolerance
-- @-node:gcross.20100228202857.1308:runEpsilonMatcher
-- @-node:gcross.20100228202857.1297:Functions
-- @-others
-- @-node:gcross.20091208183517.1416:@thin EpsilonMatcher.hs
-- @-leo
