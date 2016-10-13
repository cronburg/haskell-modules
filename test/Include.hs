{-# LANGUAGE OverloadedStrings #-}
module Include (include) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.Meta (parseDecs)

import System.Directory (listDirectory)
import qualified Data.Text as T (strip, pack, unpack)

strip = T.unpack . T.strip . T.pack

include :: QuasiQuoter
include = QuasiQuoter
  (error "parse expression")
  (error "parse pattern")
  (error "parse type")
  iparse

iparse :: String -> TH.Q [TH.Dec]
iparse fn = do
  s <- TH.runIO $ readFile $ strip fn
  let Right ds = parseDecs s
  return ds

