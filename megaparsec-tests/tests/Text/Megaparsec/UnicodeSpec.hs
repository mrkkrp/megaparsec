module Text.Megaparsec.UnicodeSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import qualified Text.Megaparsec.Unicode as Unicode

spec :: Spec
spec = do
  describe "stringLength" $ do
    it "computes correct length in the presence of wide chars" $
      Unicode.stringLength "123 구구 이면" `shouldBe` 13
    it "computes correct length in the presence of zero-width chars" $
      -- A letter, a combining acute accent that occupies no cell of its
      -- own, and a wide ideograph.
      Unicode.stringLength "e\769日" `shouldBe` 3
  describe "charLength" $ do
    it "returns 1 for ordinary chars" $
      expectLengths 1 narrowChars
    it "returns 2 for wide chars" $
      expectLengths 2 wideChars
    it "returns 0 for zero-width chars" $
      expectLengths 0 zeroWidthChars
    it "agrees with isWideChar and isZeroWidthChar for every code point" $
      -- Among other things this pins down the shortcut that both functions
      -- take for the characters below the top of the Latin-1 range.
      let inconsistent ch =
            let n = Unicode.charLength ch
             in n `notElem` [0, 1, 2]
                  || Unicode.isWideChar ch /= (n == 2)
                  || Unicode.isZeroWidthChar ch /= (n == 0)
       in take 5 (filter inconsistent [minBound .. maxBound]) `shouldBe` []
  describe "isWideChar" $ do
    it "returns False for non-wide chars" $
      Unicode.isWideChar 'a' `shouldBe` False
    it "returns True for wide chars" $
      Unicode.isWideChar '구' `shouldBe` True
    it "returns False for wide chars that combine with their neighbour" $
      -- U+302A is Wide by East Asian Width but it is a non-spacing mark, so
      -- it does not occupy two columns of its own.
      Unicode.isWideChar '\12330' `shouldBe` False
  describe "isZeroWidthChar" $ do
    it "returns False for ordinary chars" $
      Unicode.isZeroWidthChar 'a' `shouldBe` False
    it "returns True for control chars" $
      Unicode.isZeroWidthChar '\SOH' `shouldBe` True
    it "returns True for combining marks" $
      Unicode.isZeroWidthChar '\769' `shouldBe` True

-- | Check that every character in the collection has the given length,
-- naming the offending one if it does not.
expectLengths :: Int -> [(String, Char)] -> Expectation
expectLengths n cs =
  forM_ cs $ \(name, ch) ->
    (name, Unicode.charLength ch) `shouldBe` (name, n)

-- | Characters that occupy a single column.
narrowChars :: [(String, Char)]
narrowChars =
  [ ("U+0061 latin small letter a", 'a'),
    ("U+00E9 latin small letter e with acute", '\233'),
    ("U+0416 cyrillic capital letter zhe", '\1046'),
    ("U+05D0 hebrew letter alef", '\1488'),
    ("U+0E01 thai character ko kai", '\3585'),
    ("U+2192 rightwards arrow", '\8594')
  ]

-- | Characters that occupy two columns. The comments give the version of
-- the Unicode standard that assigned them, which is what makes them a
-- reasonable regression test for stale character width data.
wideChars :: [(String, Char)]
wideChars =
  [ ("U+4E00 cjk unified ideograph (1.1)", '\19968'),
    ("U+9FFF cjk unified ideograph (14.0)", '\40959'),
    ("U+4DBF cjk unified ideograph (13.0)", '\19903'),
    ("U+31BF bopomofo letter (14.0)", '\12735'),
    ("U+AC00 hangul syllable ga", '\44032'),
    ("U+FF21 fullwidth latin capital letter a", '\65313'),
    ("U+1F600 grinning face (6.1)", '\128512'),
    ("U+1F6DD playground slide (14.0)", '\128733'),
    ("U+1FAE0 melting face (14.0)", '\129760'),
    ("U+30000 cjk unified ideograph extension g (13.0)", '\196608'),
    ("U+31350 cjk unified ideograph extension h (15.0)", '\201552')
  ]

-- | Characters that occupy no columns at all.
zeroWidthChars :: [(String, Char)]
zeroWidthChars =
  [ ("U+0001 start of heading", '\SOH'),
    ("U+00AD soft hyphen", '\173'),
    ("U+0301 combining acute accent", '\769'),
    ("U+0651 arabic shadda", '\1617'),
    ("U+093C devanagari sign nukta", '\2364'),
    ("U+0BCD tamil sign virama", '\3021'),
    ("U+0E31 thai character mai han akat", '\3633'),
    ("U+1AB0 combining doubled circumflex accent", '\6832'),
    ("U+1DC0 combining dotted grave accent", '\7616'),
    ("U+200D zero width joiner", '\8205'),
    ("U+2060 word joiner", '\8288'),
    ("U+20E3 combining enclosing keycap", '\8419'),
    ("U+FE0F variation selector-16", '\65039'),
    ("U+FEFF zero width no-break space", '\65279'),
    ("U+11FF hangul jongseong ssangnieun", '\4607'),
    ("U+E0101 variation selector-18", '\917761')
  ]
