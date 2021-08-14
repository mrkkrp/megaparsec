{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Safe #-}

module Text.Megaparsec.Error
  ( ParseError,
  )
where

type role ParseError nominal nominal

data ParseError s e
