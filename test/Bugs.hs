
module Bugs
       ( bugs
       ) where

import Test.Framework

import qualified Bugs.Bug2
import qualified Bugs.Bug6
import qualified Bugs.Bug9

bugs :: [Test]
bugs = [ Bugs.Bug2.main
       , Bugs.Bug6.main
       , Bugs.Bug9.main
       ]
