-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1868:@thin test.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1870:<< Import needed modules >>
import Data.EpsilonMatcher
import Data.Map (Map)
import qualified Data.Map as Map

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
-- @-node:gcross.20091220080702.1870:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100228202857.1304:<< Tests >>
    -- @+others
    -- @+node:gcross.20100228202857.1305:null case
    [testCase "null case" $
        assertEqual
            "Is the match map correct?"
            Map.empty
            $
            runEpsilonMatcher (0 :: Int) $ do
                return ()
    -- @-node:gcross.20100228202857.1305:null case
    -- @-others
    -- @-node:gcross.20100228202857.1304:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091220080702.1868:@thin test.hs
-- @-leo
