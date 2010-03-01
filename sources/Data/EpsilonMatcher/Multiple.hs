-- @+leo-ver=4-thin
-- @+node:gcross.20100301111615.1284:@thin EpsilonMatcher/Multiple.hs
-- @@language Haskell

module Data.EpsilonMatcher.Multiple where

-- @<< Import needed modules >>
-- @+node:gcross.20100301111615.1286:<< Import needed modules >>
import Control.Arrow
import Control.Monad.State.Strict

import Data.EpsilonMatcher
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
-- @-node:gcross.20100301111615.1286:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100301111615.1287:Types
-- @+node:gcross.20100301111615.1289:MultipleEpsilonMatcherState
type MultipleEpsilonMatcherState valueType resultType = State (IntMap (EpsilonMatcher valueType)) resultType
-- @-node:gcross.20100301111615.1289:MultipleEpsilonMatcherState
-- @-node:gcross.20100301111615.1287:Types
-- @+node:gcross.20100301111615.1290:Functions
-- @+node:gcross.20100301111615.1294:matchIn
matchIn ::
    (Ord valueType, Num valueType) =>
    Int ->
    valueType ->
    IntMap (EpsilonMatcher valueType) ->
    (Int,IntMap (EpsilonMatcher valueType))
matchIn matcher_index lookup_value matchers =
    let old_matcher = fromJust . IntMap.lookup matcher_index $ matchers
        (match_key,new_matcher) = match lookup_value old_matcher
    in (match_key,IntMap.insert matcher_index new_matcher matchers)
-- @-node:gcross.20100301111615.1294:matchIn
-- @+node:gcross.20100301111615.1292:lookupMatchIn
lookupMatchIn ::
    (Ord valueType, Num valueType) =>
    Int ->
    valueType ->
    MultipleEpsilonMatcherState valueType Int
lookupMatchIn matcher_index = State . matchIn matcher_index
-- @-node:gcross.20100301111615.1292:lookupMatchIn
-- @+node:gcross.20100301111615.1296:runEpsilonMatcher
runMultipleEpsilonMatchers ::
    [valueType] ->
    MultipleEpsilonMatcherState valueType resultType ->
    (resultType,[IntMap Int])
runMultipleEpsilonMatchers tolerances stateRunner =
    second (map (getMatchMap . snd) . IntMap.toAscList)
    .
    runState stateRunner
    .
    IntMap.fromAscList
    .
    zip [0..]
    .
    map newEpsilonMatcher
    $
    tolerances
-- @-node:gcross.20100301111615.1296:runEpsilonMatcher
-- @-node:gcross.20100301111615.1290:Functions
-- @-others
-- @-node:gcross.20100301111615.1284:@thin EpsilonMatcher/Multiple.hs
-- @-leo
