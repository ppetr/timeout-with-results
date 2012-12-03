{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

module System.Timeout.Valued (MonadTimeout(..), Timeout(), timeoutVal, returning) where
{-
 Mind that (from documentation of throwTo):

There is no guarantee that the exception will be delivered promptly, although the runtime will endeavour to ensure that arbitrary delays don't occur. In GHC, an exception can only be raised when a thread reaches a safe point, where a safe point is where memory allocation occurs. Some loops do not perform any memory allocation inside the loop and therefore cannot be interrupted by a throwTo. 
-}
import Control.Monad
import Control.Monad.Reader
import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Control.Seq
import Data.Monoid
import qualified System.Timeout as T

class Monad m => MonadTimeout w m | m -> w where
    -- | Save an intermediate result of the computation.
    tell :: w -> m ()
    tell = tellWith . const
    -- | Combine an intermediate result of the computation with the current
    -- saved result (if any) and save it.
    tellWith :: (Maybe w -> w) -> m ()
    -- | Explicitly allow interrupting the computation at this point.
    -- Experimental.
    yield :: m ()

-- -----------------------------------------------------------------
    
newtype Timeout w a = Timeout { untimeout :: ReaderT ((Maybe w -> w) -> IO ()) IO a }

instance Monad (Timeout w) where
    return = Timeout . return
    (Timeout v) >>= f = Timeout (v >>= (untimeout . f))
instance MonadIO (Timeout w) where
    liftIO = Timeout . lift
instance MonadTimeout w (Timeout w) where
    tellWith f = Timeout $ ask >>= \r -> lift (r f)
    yield = liftIO C.yield

-- | Execute the given computation with a timeout limit and force
-- the result to the form defined by the given 'Strategy'.
timeoutVal
    :: Strategy w       -- ^ Evaluate values passed to 'tell' using this strategy.
    -> Int              -- ^ Timeout in microseconds.
    -> Timeout w ()     -- ^ The computation.
    -> IO (Maybe w)     -- ^ The result, or 'Nothing' if no 'tell' was called by the computation.
timeoutVal stg duration (Timeout k) = do
    mvar <- newMVar Nothing
    let save f = modifyMVar_ mvar (return . Just . withStrategy stg . f)
    T.timeout duration (runReaderT k save)
    readMVar mvar

-- | Convert a monadic computation returning a value of the result type into
-- 'm ()' so that it can be used with 'timeoutVal'. Calling 'returning k'
-- is equivalent to 'k >>= tell'.
returning :: MonadTimeout w m => m w -> m ()
returning = (>>= tell)
