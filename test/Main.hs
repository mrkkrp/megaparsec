
import Test.Framework

import Bugs ( bugs )

main :: IO ()
main = defaultMain [testGroup "Bugs" bugs]
