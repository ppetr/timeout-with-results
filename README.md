# timeout-with-results

A Haskell library that allows timeouting a computation while allowing it to
return partial results.  Useful for making AI-like algorithms that should
return the best result found within a time limit.

## Example

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Seq
import System.Timeout.Returning

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- | Loop forever, computing factorials for increasing 'n'.
longComp :: MonadTimeout String m => m ()
longComp = let loop n = tell (show $ fac n) >> loop (n + 1)
           in loop 0

-- | Print the largest factorial we were able to compute in 10ms.
main :: IO ()
main = timeoutVal rseq 10000 longComp >>= print
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

