module Main where
import Control.Monad.State.Lazy (execState, runStateT, liftIO)
import Data.List
import Control.Lens

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM


main :: IO ()
main = defaultMainWithOpts
  [ testCase      "rev"                 testRev
  ] mempty

-- Example assertion and property:
testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

