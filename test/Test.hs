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

import Language.Haskell.Modules.Parser
import Language.Haskell.Modules.Syntax
import qualified Text.Parsec.Prim   as PP

main :: IO ()
main = defaultMainWithOpts
  [ testCase      "rev"                 testRev
  , testCase      "parseModule"         testParse
  ] mempty

-- Example assertion and property:
testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

testParse :: Assertion
testParse = (PP.parse parseModule "" $ unlines [
  "module M (x,y,z) where",
  "x :: Int",
  "y :: Int",
  "z :: Int"
  ]) @?= Right testParseExp

testParseExp = Module
  { name    = Just $ MN "M"
  , exports = Just [Fncn "x", Fncn "y", Fncn "z"]
  , imports = []
  }

