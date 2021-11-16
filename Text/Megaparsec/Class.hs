{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Text.Megaparsec.Class
-- Copyright   :  © 2015–present Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of 'MonadParsec'—type class describing monads that implement
-- the full set of primitive parsers.
--
-- @since 6.5.0
module Text.Megaparsec.Class
  ( MonadParsec (..),
  )
where

import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as L
import qualified Control.Monad.RWS.Strict as S
import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as L
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import Data.Set (Set)
import Text.Megaparsec.Error
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- | Type class describing monads that implement the full set of primitive
-- parsers.
--
-- __Note__ that the following primitives are “fast” and should be taken
-- advantage of as much as possible if your aim is a fast parser: 'tokens',
-- 'takeWhileP', 'takeWhile1P', and 'takeP'.
class (Stream s, MonadPlus m) => MonadParsec e s m | m -> e s where
  -- | Stop parsing and report the 'ParseError'. This is the only way to
  -- control position of the error without manipulating the parser state
  -- manually.
  --
  -- @since 8.0.0
  parseError :: ParseError s e -> m a

  -- | The parser @'label' name p@ behaves as parser @p@, but whenever the
  -- parser @p@ fails /without consuming any input/, it replaces names of
  -- “expected” tokens with the name @name@.
  label :: String -> m a -> m a

  -- | @'hidden' p@ behaves just like parser @p@, but it doesn't show any
  -- “expected” tokens in error message when @p@ fails.
  --
  -- Please use 'hidden' instead of the old @'label' ""@ idiom.
  hidden :: m a -> m a
  hidden = label ""

  -- | The parser @'try' p@ behaves like the parser @p@, except that it
  -- backtracks the parser state when @p@ fails (either consuming input or
  -- not).
  --
  -- This combinator is used whenever arbitrary look ahead is needed. Since
  -- it pretends that it hasn't consumed any input when @p@ fails, the
  -- ('A.<|>') combinator will try its second alternative even if the first
  -- parser failed while consuming input.
  --
  -- For example, here is a parser that is supposed to parse the word “let”
  -- or the word “lexical”:
  --
  -- >>> parseTest (string "let" <|> string "lexical") "lexical"
  -- 1:1:
  -- unexpected "lex"
  -- expecting "let"
  --
  -- What happens here? The first parser consumes “le” and fails (because it
  -- doesn't see a “t”). The second parser, however, isn't tried, since the
  -- first parser has already consumed some input! 'try' fixes this behavior
  -- and allows backtracking to work:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "lexical"
  -- "lexical"
  --
  -- 'try' also improves error messages in case of overlapping alternatives,
  -- because Megaparsec's hint system can be used:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "le"
  -- 1:1:
  -- unexpected "le"
  -- expecting "let" or "lexical"
  --
  -- __Note__ that as of Megaparsec 4.4.0, 'Text.Megaparsec.Char.string'
  -- backtracks automatically (see 'tokens'), so it does not need 'try'.
  -- However, the examples above demonstrate the idea behind 'try' so well
  -- that it was decided to keep them. You still need to use 'try' when your
  -- alternatives are complex, composite parsers.
  try :: m a -> m a

  -- | If @p@ in @'lookAhead' p@ succeeds (either consuming input or not)
  -- the whole parser behaves like @p@ succeeded without consuming anything
  -- (parser state is not updated as well). If @p@ fails, 'lookAhead' has no
  -- effect, i.e. it will fail consuming input if @p@ fails consuming input.
  -- Combine with 'try' if this is undesirable.
  lookAhead :: m a -> m a

  -- | @'notFollowedBy' p@ only succeeds when the parser @p@ fails. This
  -- parser /never consumes/ any input and /never modifies/ parser state. It
  -- can be used to implement the “longest match” rule.
  notFollowedBy :: m a -> m ()

  -- | @'withRecovery' r p@ allows us to continue parsing even if the parser
  -- @p@ fails. In this case @r@ is called with the actual 'ParseError' as
  -- its argument. Typical usage is to return a value signifying failure to
  -- parse this particular object and to consume some part of the input up
  -- to the point where the next object starts.
  --
  -- Note that if @r@ fails, the original error message is reported as if
  -- without 'withRecovery'. In no way recovering parser @r@ can influence
  -- error messages.
  --
  -- @since 4.4.0
  withRecovery ::
    -- | How to recover from failure
    (ParseError s e -> m a) ->
    -- | Original parser
    m a ->
    -- | Parser that can recover from failures
    m a

  -- | @'observing' p@ allows us to “observe” failure of the @p@ parser,
  -- should it happen, without actually ending parsing but instead getting
  -- the 'ParseError' in 'Left'. On success parsed value is returned in
  -- 'Right' as usual. Note that this primitive just allows you to observe
  -- parse errors as they happen, it does not backtrack or change how the
  -- @p@ parser works in any way.
  --
  -- @since 5.1.0
  observing ::
    -- | The parser to run
    m a ->
    m (Either (ParseError s e) a)

  -- | This parser only succeeds at the end of input.
  eof :: m ()

  -- | The parser @'token' test expected@ accepts tokens for which the
  -- matching function @test@ returns 'Just' results. If 'Nothing' is
  -- returned the @expected@ set is used to report the items that were
  -- expected.
  --
  -- For example, the 'Text.Megaparsec.satisfy' parser is implemented as:
  --
  -- > satisfy f = token testToken Set.empty
  -- >   where
  -- >     testToken x = if f x then Just x else Nothing
  --
  -- __Note__: type signature of this primitive was changed in the version
  -- /7.0.0/.
  token ::
    -- | Matching function for the token to parse
    (Token s -> Maybe a) ->
    -- | Used in the error message to mention the items that were expected
    Set (ErrorItem (Token s)) ->
    m a

  -- | The parser @'tokens' test chk@ parses a chunk of input @chk@ and
  -- returns it. The supplied predicate @test@ is used to check equality of
  -- given and parsed chunks after a candidate chunk of correct length is
  -- fetched from the stream.
  --
  -- This can be used for example to write 'Text.Megaparsec.chunk':
  --
  -- > chunk = tokens (==)
  --
  -- Note that beginning from Megaparsec 4.4.0, this is an auto-backtracking
  -- primitive, which means that if it fails, it never consumes any input.
  -- This is done to make its consumption model match how error messages for
  -- this primitive are reported (which becomes an important thing as user
  -- gets more control with primitives like 'withRecovery'):
  --
  -- >>> parseTest (string "abc") "abd"
  -- 1:1:
  -- unexpected "abd"
  -- expecting "abc"
  --
  -- This means, in particular, that it's no longer necessary to use 'try'
  -- with 'tokens'-based parsers, such as 'Text.Megaparsec.Char.string' and
  -- 'Text.Megaparsec.Char.string''. This feature /does not/ affect
  -- performance in any way.
  tokens ::
    -- | Predicate to check equality of chunks
    (Tokens s -> Tokens s -> Bool) ->
    -- | Chunk of input to match against
    Tokens s ->
    m (Tokens s)

  -- | Parse /zero/ or more tokens for which the supplied predicate holds.
  -- Try to use this as much as possible because for many streams this
  -- combinator is much faster than parsers built with
  -- 'Control.Monad.Combinators.many' and 'Text.Megaparsec.satisfy'.
  --
  -- > takeWhileP (Just "foo") f = many (satisfy f <?> "foo")
  -- > takeWhileP Nothing      f = many (satisfy f)
  --
  -- The combinator never fails, although it may parse the empty chunk.
  --
  -- @since 6.0.0
  takeWhileP ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | Predicate to use to test tokens
    (Token s -> Bool) ->
    -- | A chunk of matching tokens
    m (Tokens s)

  -- | Similar to 'takeWhileP', but fails if it can't parse at least one
  -- token. Try to use this as much as possible because for many streams
  -- this combinator is much faster than parsers built with
  -- 'Control.Monad.Combinators.some' and 'Text.Megaparsec.satisfy'.
  --
  -- > takeWhile1P (Just "foo") f = some (satisfy f <?> "foo")
  -- > takeWhile1P Nothing      f = some (satisfy f)
  --
  -- Note that the combinator either succeeds or fails without consuming any
  -- input, so 'try' is not necessary with it.
  --
  -- @since 6.0.0
  takeWhile1P ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | Predicate to use to test tokens
    (Token s -> Bool) ->
    -- | A chunk of matching tokens
    m (Tokens s)

  -- | Extract the specified number of tokens from the input stream and
  -- return them packed as a chunk of stream. If there is not enough tokens
  -- in the stream, a parse error will be signaled. It's guaranteed that if
  -- the parser succeeds, the requested number of tokens will be returned.
  --
  -- The parser is roughly equivalent to:
  --
  -- > takeP (Just "foo") n = count n (anySingle <?> "foo")
  -- > takeP Nothing      n = count n anySingle
  --
  -- Note that if the combinator fails due to insufficient number of tokens
  -- in the input stream, it backtracks automatically. No 'try' is necessary
  -- with 'takeP'.
  --
  -- @since 6.0.0
  takeP ::
    -- | Name for a single token in the row
    Maybe String ->
    -- | How many tokens to extract
    Int ->
    -- | A chunk of matching tokens
    m (Tokens s)

  -- | Return the full parser state as a 'State' record.
  getParserState :: m (State s e)

  -- | @'updateParserState' f@ applies the function @f@ to the parser state.
  updateParserState :: (State s e -> State s e) -> m ()

----------------------------------------------------------------------------
-- Lifting through MTL

instance MonadParsec e s m => MonadParsec e s (L.StateT st m) where
  parseError e = lift (parseError e)
  label n (L.StateT m) = L.StateT $ label n . m
  try (L.StateT m) = L.StateT $ try . m
  lookAhead (L.StateT m) = L.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (L.StateT m) = L.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((), s)
  withRecovery r (L.StateT m) = L.StateT $ \s ->
    withRecovery (\e -> L.runStateT (r e) s) (m s)
  observing (L.StateT m) = L.StateT $ \s ->
    fixs s <$> observing (m s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (S.StateT st m) where
  parseError e = lift (parseError e)
  label n (S.StateT m) = S.StateT $ label n . m
  try (S.StateT m) = S.StateT $ try . m
  lookAhead (S.StateT m) = S.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (S.StateT m) = S.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((), s)
  withRecovery r (S.StateT m) = S.StateT $ \s ->
    withRecovery (\e -> S.runStateT (r e) s) (m s)
  observing (S.StateT m) = S.StateT $ \s ->
    fixs s <$> observing (m s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (L.ReaderT r m) where
  parseError e = lift (parseError e)
  label n (L.ReaderT m) = L.ReaderT $ label n . m
  try (L.ReaderT m) = L.ReaderT $ try . m
  lookAhead (L.ReaderT m) = L.ReaderT $ lookAhead . m
  notFollowedBy (L.ReaderT m) = L.ReaderT $ notFollowedBy . m
  withRecovery r (L.ReaderT m) = L.ReaderT $ \s ->
    withRecovery (\e -> L.runReaderT (r e) s) (m s)
  observing (L.ReaderT m) = L.ReaderT $ observing . m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (L.WriterT w m) where
  parseError e = lift (parseError e)
  label n (L.WriterT m) = L.WriterT $ label n m
  try (L.WriterT m) = L.WriterT $ try m
  lookAhead (L.WriterT m) =
    L.WriterT $
      (,mempty) . fst <$> lookAhead m
  notFollowedBy (L.WriterT m) =
    L.WriterT $
      (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (L.WriterT m) =
    L.WriterT $
      withRecovery (L.runWriterT . r) m
  observing (L.WriterT m) =
    L.WriterT $
      fixs mempty <$> observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (S.WriterT w m) where
  parseError e = lift (parseError e)
  label n (S.WriterT m) = S.WriterT $ label n m
  try (S.WriterT m) = S.WriterT $ try m
  lookAhead (S.WriterT m) =
    S.WriterT $
      (,mempty) . fst <$> lookAhead m
  notFollowedBy (S.WriterT m) =
    S.WriterT $
      (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (S.WriterT m) =
    S.WriterT $
      withRecovery (S.runWriterT . r) m
  observing (S.WriterT m) =
    S.WriterT $
      fixs mempty <$> observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

-- | @since 5.2.0
instance (Monoid w, MonadParsec e s m) => MonadParsec e s (L.RWST r w st m) where
  parseError e = lift (parseError e)
  label n (L.RWST m) = L.RWST $ \r s -> label n (m r s)
  try (L.RWST m) = L.RWST $ \r s -> try (m r s)
  lookAhead (L.RWST m) = L.RWST $ \r s -> do
    (x, _, _) <- lookAhead (m r s)
    return (x, s, mempty)
  notFollowedBy (L.RWST m) = L.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((), s, mempty)
  withRecovery n (L.RWST m) = L.RWST $ \r s ->
    withRecovery (\e -> L.runRWST (n e) r s) (m r s)
  observing (L.RWST m) = L.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

-- | @since 5.2.0
instance (Monoid w, MonadParsec e s m) => MonadParsec e s (S.RWST r w st m) where
  parseError e = lift (parseError e)
  label n (S.RWST m) = S.RWST $ \r s -> label n (m r s)
  try (S.RWST m) = S.RWST $ \r s -> try (m r s)
  lookAhead (S.RWST m) = S.RWST $ \r s -> do
    (x, _, _) <- lookAhead (m r s)
    return (x, s, mempty)
  notFollowedBy (S.RWST m) = S.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((), s, mempty)
  withRecovery n (S.RWST m) = S.RWST $ \r s ->
    withRecovery (\e -> S.runRWST (n e) r s) (m r s)
  observing (S.RWST m) = S.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift (tokens e ts)
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (IdentityT m) where
  parseError e = lift (parseError e)
  label n (IdentityT m) = IdentityT $ label n m
  try = IdentityT . try . runIdentityT
  lookAhead (IdentityT m) = IdentityT $ lookAhead m
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  withRecovery r (IdentityT m) =
    IdentityT $
      withRecovery (runIdentityT . r) m
  observing (IdentityT m) = IdentityT $ observing m
  eof = lift eof
  token test mt = lift (token test mt)
  tokens e ts = lift $ tokens e ts
  takeWhileP l f = lift (takeWhileP l f)
  takeWhile1P l f = lift (takeWhile1P l f)
  takeP l n = lift (takeP l n)
  getParserState = lift getParserState
  updateParserState f = lift $ updateParserState f

fixs :: s -> Either a (b, s) -> (Either a b, s)
fixs s (Left a) = (Left a, s)
fixs _ (Right (b, s)) = (Right b, s)
{-# INLINE fixs #-}

fixs' :: Monoid w => s -> Either a (b, s, w) -> (Either a b, s, w)
fixs' s (Left a) = (Left a, s, mempty)
fixs' _ (Right (b, s, w)) = (Right b, s, w)
{-# INLINE fixs' #-}
