module Main (main) where

import Test.Hspec.Runner
import Spec (spec)

main :: IO ()
main = hspecWith defaultConfig { configQuickCheckMaxSuccess = Just 1000 } spec
