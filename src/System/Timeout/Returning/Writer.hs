{-
    This file is part of timeout-with-results.

    timeout-with-results is free software: you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the License,
    or (at your option) any later version.

    timeout-with-results is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
    License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with timeout-with-results.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances,
    FlexibleContexts #-}

{- | Defines a writer monad for computations that can be interrupted by a
   timeout. Written partial results are combined using their monoid operation
   and if a timeout occurs, the result is returned.
  
   Several utility monoids that force their values to /weak head normal form/
   or to /normal form/ are provided.
 -}
module System.Timeout.Returning.Writer (
    MonadWriter(..),
    MonadTimeout(..),
    MonadTimeoutWriter(..),
    TimeoutWriter(),
    runTimeout,
    -- helpers:
    withTimeoutWriter,
    Last'(..),
    SeqMax(..),
    NFMonoid(..),
    defaultListen,
    defaultPass
) where
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Control.DeepSeq (NFData(..))
import Control.Seq
import Data.Monoid
import qualified System.Timeout as T


-- | Monad for computations that can save partial results
-- of type @w@ during their evaluation.
class (Monad m) => MonadTimeout w m | m -> w where
    -- | Store a new partial result. The precise semantics of what happens
    -- with the written value is by intent unspecified and left to
    -- be decided by implementations.
    partialResult :: w -> m ()
    -- | Explicitly allow interrupting the computation at this point.
    -- Experimental.
    yield :: m ()
    yield = return ()

-- | Extends 'MonadTimeout' to 'MonadWriter'. Written values are combined
-- together using @w@'s monoid. In addition, allows to run a sub-computation
-- in a contained environment, without affecting the current partial result.
class (Monoid w, MonadTimeout w m, MonadWriter w m)
        => MonadTimeoutWriter w m | m -> w where
    -- | Runs the given computation separately and return its result.
    -- Does not modify the current result!
    contained :: m r -> m (r, w)
    contained k = do
        ~(_, zero) <- listen (return ())
        pass (listen k >>= \x -> return (x, const zero))

-- | A default implementation of 'listen' using 'contained'.
-- Useful only for authors of implementations of 'MonadTimeout'.
defaultListen :: MonadTimeoutWriter w m => m a -> m (a, w)
defaultListen k = do
    (x, w) <- contained k
    tell w
    return (x, w)

-- | A default implementation of 'pass' using 'contained'.
-- Useful only for authors of implementations of 'MonadTimeout'.
defaultPass :: MonadTimeoutWriter w m => m (a, w -> w) -> m a
defaultPass k = do
    ((x, f), w) <- contained k
    tell (f w)
    return x

-- -----------------------------------------------------------------

-- | An 'IO'-based implementation of 'MonadTimeoutWriter'.  Calling
-- 'partialResult' (or equivalently 'tell') combines the value with any
-- previously written values using @w@'s monoidal operation.
newtype TimeoutWriter w a
    = TimeoutWriter { getTimeoutWriter :: ReaderT (w -> IO ()) IO a }

instance Functor (TimeoutWriter w) where
    fmap = liftM
instance Applicative (TimeoutWriter w) where
    pure  = return
    (<*>) = ap
instance Monad (TimeoutWriter w) where
    return = TimeoutWriter . return
    (TimeoutWriter v) >>= f = TimeoutWriter (v >>= (getTimeoutWriter . f))
instance MonadIO (TimeoutWriter w) where
    liftIO = TimeoutWriter . lift
instance Monoid w => MonadWriter w (TimeoutWriter w) where
    tell = partialResult
    listen = defaultListen
    pass = defaultPass
instance Monoid w => MonadTimeout w (TimeoutWriter w) where
    partialResult x = TimeoutWriter $ ask >>= \r -> lift (r x)
    yield = liftIO C.yield
instance Monoid w => MonadTimeoutWriter w (TimeoutWriter w) where
    contained = liftIO . runTimeoutInternal id


-- | Modify written values using the given function.
withTimeoutWriter :: (w' -> w) -> (TimeoutWriter w' a -> TimeoutWriter w a)
withTimeoutWriter f (TimeoutWriter k) = TimeoutWriter $ withReaderT (. f) k


-- | Execute the given computation with a timeout limit.  Each time a value
-- is written, the result of 'mappend' with the previous one is evaluated to
-- /weak head normal form/.
runTimeout
    :: Monoid w
    => Int              -- ^ TimeoutWriter in microseconds.
    -> TimeoutWriter w r      -- ^ The computation.
    -> IO (Maybe r, w)  -- ^ The final result (if available) and the saved
                        -- partial result.
runTimeout duration = runTimeoutInternal (T.timeout duration)

runTimeoutInternal
    :: Monoid w
    => (IO r -> IO a)           -- ^ What to do with the computation.
    -> TimeoutWriter w r              -- ^ The computation to evaluate.
    -> IO (a, w)
runTimeoutInternal run (TimeoutWriter k) = do
    mvar <- newMVar mempty
    let save x = modifyMVar_ mvar (return . withStrategy rseq . (`mappend` x))
    r <- run (runReaderT k save)
    w <- takeMVar mvar
    return (r, w)


sseq :: Strategy a -> a -> b -> b
sseq s x y = s x `seq` y

-- | A monoid equivalent to 'Last'. In addition, it forces evaluation of
-- values inside 'Maybe' using 'rseq'. This means that when it is used in
-- 'runTimeout', the computations will be forced in the producing thread,
-- not in the consuming one. If you want to force evaluation to NF, wrap
-- it inside 'NFMonoid'.
newtype Last' a = Last' { getLast' :: Maybe a }
  deriving (Eq, Ord, Show, Read)
instance Functor Last' where
    fmap f (Last' x) = Last' $ fmap f x
instance NFData a => NFData (Last' a) where
    rnf (Last' x) = rnf x
instance Monoid (Last' a) where
    mempty = Last' Nothing
    mappend (Last' x) (Last' y)
        = Last' $ getLast (Last x `mappend` Last y)

-- | A monoid whose 'mappend' picks the grater value according to the second
-- field of the tuple. @SeqMax Nothing@ is the least element of the
-- ordering. If the second fields are the same, the left value is preferred.
-- In addition, the first field of the selected tuple is forced to evaluate
-- using 'rseq'.
newtype SeqMax a b = SeqMax (Maybe (a, b))
  deriving (Eq, Ord, Show, Read)
instance Functor (SeqMax a) where
    fmap f (SeqMax x) = SeqMax $ fmap (fmap f) x
instance (NFData a, NFData b) => NFData (SeqMax a b) where
    rnf (SeqMax x) = rnf x
instance (Ord b) => Monoid (SeqMax a b) where
    mempty = SeqMax Nothing
    mappend (SeqMax Nothing) s    = s
    mappend s (SeqMax Nothing)    = s
    mappend m1@(SeqMax (Just (r1, x1))) m2@(SeqMax (Just (r2, x2)))
        | x1 >= x2  = sseq rseq r1 m1
        | otherwise = sseq rseq r2 m2

-- | A wrapper monoid that forces each result of 'mappend'
-- to /normal form/'
newtype NFMonoid a = NFMonoid { getNFMonoid :: a }
  deriving (Eq, Ord, Show, Read, Bounded)
instance Functor NFMonoid where
    fmap f (NFMonoid x) = NFMonoid (f x)
instance (NFData a, Monoid a) => Monoid (NFMonoid a) where
    mempty = NFMonoid mempty
    mappend (NFMonoid x) (NFMonoid y)
        = NFMonoid ((x `mappend` y) `using` rdeepseq)

