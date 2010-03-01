-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1868:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100228202857.1316:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100228202857.1316:<< Language extensions >>
-- @nl
-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1870:<< Import needed modules >>
import Data.EpsilonMatcher
import Data.Map (Map)
import qualified Data.Map as Map

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Debug.Trace
-- @-node:gcross.20091220080702.1870:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100301111615.1274:Functions
-- @+node:gcross.20100301111615.1275:echo
echo :: Show a => a -> a
echo x = trace (show x) x
-- @-node:gcross.20100301111615.1275:echo
-- @-node:gcross.20100301111615.1274:Functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100228202857.1304:<< Tests >>
    -- @+others
    -- @+node:gcross.20100228202857.1305:null case
    [testCase "null case" $
        assertEqual
            "Is the match map correct?"
            ((),Map.empty)
            $
            runEpsilonMatcher (0 :: Int) $ do
                return ()
    -- @-node:gcross.20100228202857.1305:null case
    -- @+node:gcross.20100228202857.1315:singleton case
    ,testProperty "singleton case" $ \(value :: Int) ->
        (0,Map.singleton 0 0)
        ==
        (runEpsilonMatcher (0 :: Int) $ do
            lookupMatch value
        )
    -- @-node:gcross.20100228202857.1315:singleton case
    -- @+node:gcross.20100301111615.1273:duo case
    ,testProperty "duo case" $ \(value1 :: Int) (value2 :: Int) ->
        (case value1 `compare` value2 of
            LT -> (((0,1),Map.fromList [(0,0),(1,1)]) ==)
            GT -> (((0,1),Map.fromList [(0,1),(1,0)]) ==)
            EQ -> (((0,0),Map.fromList [(0,0)]) ==)
        )
        $
        (runEpsilonMatcher (0 :: Int) $ do
            key1 <- lookupMatch value1
            key2 <- lookupMatch value2
            return (key1,key2)
        )
    -- @-node:gcross.20100301111615.1273:duo case
    -- @-others
    -- @-node:gcross.20100228202857.1304:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091220080702.1868:@thin test.hs
-- @-leo
