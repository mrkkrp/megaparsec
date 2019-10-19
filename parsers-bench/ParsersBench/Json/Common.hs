{-# LANGUAGE DeriveGeneric #-}

module ParsersBench.Json.Common
  ( Result (..)
  , Object
  , Array
  , Value (..) )
where

import Control.DeepSeq
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import qualified Data.HashMap.Strict as H

data Result a
  = Error String
  | Success a
  deriving (Eq, Show)

type Object = H.HashMap Text Value

type Array = Vector Value

data Value
  = Object !Object
  | Array  !Array
  | String !Text
  | Number !Scientific
  | Bool   !Bool
  | Null
  deriving (Eq, Show, Generic)

instance NFData Value
