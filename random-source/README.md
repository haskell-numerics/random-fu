It is better to use the
[random](https://hackage.haskell.org/package/random "well maintained")
package and the source of randomness.

Here's what you might have written before:

```
import qualified Data.Random as Random
import qualified Data.Random.Distribution.Bernoulli as Bernoulli

main :: IO ()
main = do
  x <- Random.sample $ Bernoulli.Bernoulli (0.5 :: Double) :: IO Double
  print x
```

And here's what you should write now (but there are many other options):

```
import qualified System.Random.Stateful as Random.Stateful
import qualified Data.Random as Random
import qualified Data.Random.Distribution.Bernoulli as Bernoulli
import qualified Control.Monad.Reader as Reader

main :: IO ()
main = do
  stdgen <- Random.Stateful.newIOGenM =<< Random.Stateful.newStdGen
  x <- Reader.runReaderT (Random.sample $ Bernoulli.Bernoulli (0.5 :: Double)) stdgen
        :: IO Double
  print x
```
