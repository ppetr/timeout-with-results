# timeout-with-results

A Haskell library that allows timeouting a computation while allowing it to
return partial results.  Useful for making AI-like algorithms that should
return the best result found within a time limit.

## Examples

### Computing pairs of prime twins

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Data.Numbers.Primes -- package 'primes'
import System.Timeout.Returning

-- | Loop forever, outputting prime twins.
primeTwins :: MonadTimeout (Integer, Integer) m => [Integer] -> m ()
primeTwins (p : ps@(p' : _))
    | p' == p + 2   = tell (p, p') >> primeTwins ps
    | otherwise     = primeTwins ps

-- | Print the largest pair of prime twins we were able to compute in 100ms.
main :: IO ()
main = runTimeout rseq 100000 (primeTwins primes) >>= print
```

### Number guessing game

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import System.Timeout.Returning

-- | Let the user guess until she hits the number.
guess :: (MonadIO m, MonadTimeout Int m)
      => Int            -- ^ The number to be guessed.
      -> m ()
guess n = loop
  where
    loop = do
        i <- liftIO $ putStr "Guess: " >> readLn
        -- If there was no previous guess, save @i@.
        -- Otherwise, find the closest guess and save it.
        tellWith $ maybe i (closer i)
        case () of
            _ | i == n    -> return ()
              | i <  n    -> liftIO (putStrLn "Guess larger.")  >> loop
              | i >  n    -> liftIO (putStrLn "Guess smaller.") >> loop
    -- Returns the number closer to @n@.
    closer i j | abs (i-n) <= abs (j-n)     = i
               | otherwise                  = j


-- | Guess the number
main :: IO ()
main = do
    let limit = 10
    putStrLn "Guess a number from 1 to 100."
    putStrLn $ "You have " ++ show limit ++ " seconds."
    n <- randomRIO (1, 100)
    r <- runTimeout rseq (limit * (10^6)) (guess n)
    putStr "The number was: " >> print n
    if maybe False (== n) r
        then putStrLn "You win!"
        else putStr "Time's up, you lose. Your best guess: " >> print r
```

# Copyright

Copyright 2012, Petr Pudlák

Contact: [petr.pudlak.name](http://petr.pudlak.name/).

![LGPLv3](https://www.gnu.org/graphics/lgplv3-88x31.png)

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.

