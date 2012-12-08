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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

module System.Timeout.Returning (
    MonadTimeout(..),
    Timeout(),
    runTimeout,
    -- helpers:
    returning,
    seqLast,
    -- re-exports:
    rseq, rdeepseq
) where
{-
 Mind that (from documentation of throwTo):

There is no guarantee that the exception will be delivered promptly, although the runtime will endeavour to ensure that arbitrary delays don't occur. In GHC, an exception can only be raised when a thread reaches a safe point, where a safe point is where memory allocation occurs. Some loops do not perform any memory allocation inside the loop and therefore cannot be interrupted by a throwTo. 
-}
import Control.Monad
import Control.Monad.Reader
import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Control.Seq
import qualified Data.Foldable as F
import Data.Monoid
import qualified System.Timeout as T

-- | Monad for computations that can save partial results
-- of type @w@ during their evaluation.
class Monad m => MonadTimeout w m | m -> w where
    -- | Save an intermediate result of the computation.
    tell :: w -> m ()
    --tell = tellWith . const
    -- | Combine an intermediate result of the computation with the current
    -- saved result (if any) and save it.
    --tellWith :: (Maybe w -> w) -> m ()
    -- | Explicitly allow interrupting the computation at this point.
    -- Experimental.
    yield :: m ()

-- -----------------------------------------------------------------

-- | An 'IO'-based implementation of 'MonadTimeout'.
newtype Timeout w a = Timeout { untimeout :: ReaderT (w -> IO ()) IO a }

instance Monad (Timeout w) where
    return = Timeout . return
    (Timeout v) >>= f = Timeout (v >>= (untimeout . f))
instance MonadIO (Timeout w) where
    liftIO = Timeout . lift
instance Monoid w => MonadTimeout w (Timeout w) where
    tell x = Timeout $ ask >>= \r -> lift (r x)
    --tellWith f = Timeout $ ask >>= \r -> lift (r f)
    yield = liftIO C.yield


instance Functor Last where
    fmap f (Last x)     = Last $ fmap f x
instance F.Foldable Last where
    foldr f z (Last x)  = F.foldr f z x

seqLast :: Strategy a -> Strategy (Last a)
seqLast = seqFoldable

withTimeout :: (w' -> w) -> (Timeout w' a -> Timeout w a)
withTimeout f (Timeout k) = Timeout $ withReaderT (. f) k

-- | Execute the given computation with a timeout limit and force
-- the result to the form defined by the given 'Strategy'.
-- In most cases, the strategy will be either 'rseq' or 'rdeepseq'.
-- This ensures that the computation is actually performed by the
-- given computation, not by the consuming thread.
runTimeout'
    :: Strategy w       -- ^ Evaluate values passed to 'tell' using this strategy.
    -> Int              -- ^ Timeout in microseconds.
    -> Timeout w ()     -- ^ The computation.
    -> IO (Maybe w)     -- ^ The result, or 'Nothing' if no 'tell' was called by the computation.
runTimeout' stg duration k = do
    r <- runTimeout (seqLast stg) duration (withTimeout (Last . Just) k)
    return $ either getLast (const Nothing) r
{-
    mvar <- newMVar Nothing
    let save f = modifyMVar_ mvar (return . Just . withStrategy stg . f)
    T.timeout duration (runReaderT k save)
    readMVar mvar
-}

-- | Execute the given computation with a timeout limit and force
-- the result to the form defined by the given 'Strategy'.
-- In most cases, the strategy will be either 'rseq' or 'rdeepseq'.
-- This ensures that the computation is actually performed by the
-- given computation, not by the consuming thread.
runTimeout
    :: Monoid w
    => Strategy w       -- ^ Evaluate values passed to 'tell' using this strategy.
    -> Int              -- ^ Timeout in microseconds.
    -> Timeout w r      -- ^ The computation.
    -> IO (Either w r)  -- ^ Either the final result or
                        -- the last partial result saved.
runTimeout stg duration (Timeout k) = do
    mvar <- newMVar (Right mempty)
    let save x = modifyMVar_ mvar (return . liftM (withStrategy stg . (`mappend` x)))
    T.timeout duration (runReaderT k save >>= \r -> modifyMVar_ mvar (\_ -> return $ Left r))
    liftM (either Right Left) $ readMVar mvar

-- | Convert a monadic computation returning a value of the result type into
-- 'm ()' so that it can be used with 'runTimeout'. Calling 'returning k'
-- is equivalent to 'k >>= tell'.
returning :: MonadTimeout w m => m w -> m ()
returning = (>>= tell)
