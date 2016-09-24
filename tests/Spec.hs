{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
#else

module Spec (main) where

main :: IO ()
main = return ()
#endif
