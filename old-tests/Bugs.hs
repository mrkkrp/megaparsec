
module Bugs (bugs) where

import Test.Framework

import qualified Bugs.Bug2
import qualified Bugs.Bug6
-- import qualified Bugs.Bug9
import qualified Bugs.Bug35
import qualified Bugs.Bug39

bugs :: [Test]
bugs = [ Bugs.Bug2.main
       , Bugs.Bug6.main
       -- , Bugs.Bug9.main FIXME enable me when my time comes
       , Bugs.Bug35.main
       , Bugs.Bug39.main ]
