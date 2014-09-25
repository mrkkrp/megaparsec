
module Bugs
       ( bugs
       ) where

import Test.HUnit hiding ( Test )
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Bugs.Bug2
import qualified Bugs.Bug6
import qualified Bugs.Bug9

bugs :: [Test]
bugs = [ Bugs.Bug2.main
       , Bugs.Bug6.main
       , Bugs.Bug9.main
       ]
