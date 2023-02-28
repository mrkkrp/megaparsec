{-# LANGUAGE RoleAnnotations #-}

module Text.Megaparsec.Internal
  ( Reply,
  )
where

type role Reply nominal nominal representational

data Reply e s a
