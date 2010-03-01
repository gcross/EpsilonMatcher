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
    -- @+node:gcross.20100301111615.1277:trio case
    ,testProperty "trio case" $ \(value1 :: Int) (value2 :: Int) (value3 :: Int)->
        (case (value1 `compare` value2,value2 `compare` value3,value1 `compare` value3) of
            (EQ,EQ,_) -> (((0,0,0),Map.fromList [(0,0)]) ==)
            (LT,LT,_) -> (((0,1,2),Map.fromList [(0,0),(1,1),(2,2)]) ==)
            (LT,EQ,_) -> (((0,1,1),Map.fromList [(0,0),(1,1)]) ==)
            (EQ,LT,_) -> (((0,0,1),Map.fromList [(0,0),(1,1)]) ==)
            (GT,GT,_) -> (((0,1,2),Map.fromList [(0,2),(1,1),(2,0)]) ==)
            (GT,EQ,_) -> (((0,1,1),Map.fromList [(0,1),(1,0)]) ==)
            (EQ,GT,_) -> (((0,0,1),Map.fromList [(0,1),(1,0)]) ==)
            (GT,LT,EQ) -> (((0,1,0),Map.fromList [(0,1),(1,0)]) ==)
            (GT,LT,LT) -> (((0,1,2),Map.fromList [(0,1),(1,0),(2,2)]) ==)
            (GT,LT,GT) -> (((0,1,2),Map.fromList [(0,2),(1,0),(2,1)]) ==)
            (LT,GT,EQ) -> (((0,1,0),Map.fromList [(0,0),(1,1)]) ==)
            (LT,GT,LT) -> (((0,1,2),Map.fromList [(0,0),(1,2),(2,1)]) ==)
            (LT,GT,GT) -> (((0,1,2),Map.fromList [(0,1),(1,2),(2,0)]) ==)
        )
        (runEpsilonMatcher (0 :: Int) $ do
            key1 <- lookupMatch value1
            key2 <- lookupMatch value2
            key3 <- lookupMatch value3
            return (key1,key2,key3)
        )
    -- @-node:gcross.20100301111615.1277:trio case
    -- @-others
    -- @-node:gcross.20100228202857.1304:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091220080702.1868:@thin test.hs
-- @-leo
