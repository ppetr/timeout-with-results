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

-- | Loop forever, computing prime twins.
primeTwins :: MonadTimeout (Integer, Integer) m => [Integer] -> m (Maybe (Integer,Integer))
primeTwins (p : ps@(p' : _))
    | p' == p + 2   = partialResult (p, p') >> primeTwins ps
    | otherwise     = primeTwins ps

-- | Print the largest pair of prime twins we were able to compute in 100ms.
main :: IO ()
main = runTimeoutNF 100000 (primeTwins primes) >>= print
```

### Number guessing game

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import System.Timeout.Returning.Writer

-- | Let the user guess until she hits the number.
guess :: (MonadIO m, MonadWriter [Int] m)
      => Int            -- ^ The number to be guessed.
      -> m ()
guess n = loop
  where
    loop = do
        is <- liftIO $ putStr "Guess: " >> liftM reads getLine
        case is of
            ((i,_) : _) -> do
                tell [i]
                case i `compare` n of
                    EQ  -> return ()
                    LT  -> liftIO (putStrLn "Guess larger.")  >> loop
                    GT  -> liftIO (putStrLn "Guess smaller.") >> loop
            _ -> liftIO (putStrLn "Invalid number.") >> loop


-- | Guess a number.
main :: IO ()
main = do
    let limit = 20
    putStrLn "Guess a number from 1 to 100."
    putStrLn $ "You have " ++ show limit ++ " seconds."
    n <- randomRIO (1, 100)
    (r, w) <- runTimeout (limit * (10^6)) (guess n)
    putStrLn ""
    putStr "The number was: " >> print n
    case r of
        Just _      -> putStrLn "You win!"
        otherwise   -> putStr "Time's up, you lose. Your guesses: " >> print w
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

