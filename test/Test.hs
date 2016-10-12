module Main where
import Control.Monad.State.Lazy (execState, runStateT, liftIO)
import Data.List
import Control.Lens
import System.Directory
import qualified Data.Set as Set
import Data.Either (rights, lefts)

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
  , testCase      "parseModule"       $ testParse "test/ex00/" [ex00_exp]
  ] mempty

-- Example assertion and property:
testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

ex00_exp =
  Module
    { name    = MN "M"
    , srcdir  = "test/ex00/"
    , exports = Just [Fncn "x", Fncn "y", Fncn "z"]
    , imports = []
    , env     = []
  }

testParse :: FilePath -> [Module] -> Assertion
testParse dir ms_exp = do
  ms <- parsePackage dir
  ms @?= Right ms_exp

