-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec, main module.
--
-- Copyright © 2015 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the names of the copyright holders nor the names of
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
--
-- This software is provided by the copyright holders “as is” and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

module Main (main) where

import Test.Framework (defaultMain)

import qualified Pos
import qualified Error
import qualified Prim
import qualified Combinator
import qualified Char
import qualified Expr
import qualified Perm
import qualified Lexer

main :: IO ()
main = defaultMain
       [ Pos.tests
       , Error.tests
       , Prim.tests
       , Combinator.tests
       , Char.tests
       , Expr.tests
       , Perm.tests
       , Lexer.tests ]
