
import Test.Framework

import Tokens ( tokensTests )

main :: IO ()
main = do
  defaultMain
    [ testGroup "Text.Parsec.Tokens" tokensTests
    ]