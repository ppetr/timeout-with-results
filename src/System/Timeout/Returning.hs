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

{- | Defines a simple monad for computations that can be interrupted by a
   timeout, and save partial results before that.
  
   If you need a more powerful mechanism, where you can retrieve and combine
   previously saved partial results, use module
   "System.Timeout.Returning.Writer".
  
   Mind that (from documentation of 'Control.Exception.Base.throwTo'): \"There
   is no guarantee that the exception will be delivered promptly, although
   the runtime will endeavour to ensure that arbitrary delays don't occur. In
   GHC, an exception can only be raised when a thread reaches a safe point,
   where a safe point is where memory allocation occurs. Some loops do not
   perform any memory allocation inside the loop and therefore cannot be
   interrupted by a @throwTo@.\"
 -}
module System.Timeout.Returning (
    MonadTimeout(..),
    Timeout(),
    runTimeoutNF,
    runTimeoutWHNF,
) where
{-
-}
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Monad.Writer (MonadIO(..))

import System.Timeout.Returning.Writer


-- | An 'IO'-based implementation of 'MonadTimeout'.
-- Calling 'partialResult' replaces any previously written value
-- with the new one.
newtype Timeout w a = Timeout { getTimeout :: TimeoutWriter (Last' w) a }

instance Functor (Timeout w) where
    fmap = liftM
instance Applicative (Timeout w) where
    pure  = return
    (<*>) = ap
instance Monad (Timeout w) where
    return = Timeout . return
    (Timeout v) >>= f = Timeout (v >>= (getTimeout . f))
instance MonadIO (Timeout w) where
    liftIO = Timeout . liftIO
instance MonadTimeout w (Timeout w) where
    partialResult = Timeout . tell . Last' . Just
    yield = Timeout yield

-- | Runs the given simple computation with the given timeout.  If the
-- computation returns a value, the value is returned.  If it doesn't or
-- times out, the last partial result written by 'partialResult' is returned.
-- Each partial result is converted to /weak head normal form/ prior being
-- saved.
runTimeoutWHNF
    :: Int                  -- ^ TimeoutWriter in microseconds.
    -> Timeout w (Maybe w)  -- ^ The computation.
    -> IO (Maybe w)         -- ^ The result, or 'Nothing' if no value was
                            -- returned.
runTimeoutWHNF duration (Timeout k) = do
    (r, w) <- runTimeout duration k
    return $ join r `mplus` (getLast' w)

-- | Runs the given simple computation with the given timeout.  If the
-- computation returns a value, the value is returned.  If it doesn't or
-- times out, the last partial result written by 'partialResult' is returned.
-- Each partial result is converted to /normal form/ prior being saved.
runTimeoutNF
    :: NFData w
    => Int                  -- ^ TimeoutWriter in microseconds.
    -> Timeout w (Maybe w)  -- ^ The computation.
    -> IO (Maybe w)         -- ^ The result, or 'Nothing' if no value was
                            -- returned.
runTimeoutNF duration (Timeout k) = do
    (r, w) <- runTimeout duration (withTimeoutWriter NFMonoid k)
    return $ join r `mplus` (getLast' . getNFMonoid $ w)
