{-# LANGUAGE RoleAnnotations #-}

module Text.Megaparsec.Error
  ( ParseError,
  )
where

type role ParseError nominal nominal

data ParseError s e
