module Main (main) where

import Test.Hspec
import Test.Hspec.Runner

import qualified Text.MegaparsecSpec

import qualified Text.Megaparsec.ByteSpec
import qualified Text.Megaparsec.Byte.LexerSpec

import qualified Text.Megaparsec.CharSpec
import qualified Text.Megaparsec.Char.LexerSpec

import qualified Text.Megaparsec.DebugSpec
import qualified Text.Megaparsec.ErrorSpec
import qualified Text.Megaparsec.PosSpec
import qualified Text.Megaparsec.StreamSpec

-- | Here we define 'spec' manually rather than with @hspec-discover@ so we
-- can disable some tests quickly by commenting out the corresponding lines
-- here (running all the tests takes a while now).

spec :: Spec
spec = do

  Text.MegaparsecSpec.spec

  Text.Megaparsec.ByteSpec.spec
  Text.Megaparsec.Byte.LexerSpec.spec

  Text.Megaparsec.CharSpec.spec
  Text.Megaparsec.Char.LexerSpec.spec

  Text.Megaparsec.DebugSpec.spec
  Text.Megaparsec.ErrorSpec.spec
  Text.Megaparsec.PosSpec.spec
  Text.Megaparsec.StreamSpec.spec

main :: IO ()
main = hspecWith defaultConfig { configQuickCheckMaxSuccess = Just 1000 } spec
