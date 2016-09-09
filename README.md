# fsnotify-conduit

Get filesystem notifications as a stream of events, using the conduit
package to handle the stream. This uses the
[fsnotify package](https://www.stackage.org/package/fsnotify), which
uses OS-specific file notification APIs for efficiency. Simple usage
example, a program which will print all events for the given directory
tree:

``` haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.15
     --install-ghc
     runghc
     --package fsnotify-conduit
     --package conduit-combinators
 -}

import Conduit
import Data.Conduit.FSNotify
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dir <-
        case args of
            [dir] -> return dir
            _ -> error $ "Expected one argument (directory to watch)"
    runResourceT
        $ sourceFileChanges (setRelative False $ mkFileChangeSettings dir)
       $$ mapM_C (liftIO . print)
```

There's also the option of using the
[`Data.Acquire` module](https://www.stackage.org/haddock/lts-6.16/resourcet-1.1.7.5/Data-Acquire.html)
approach in place of `ResourceT`, which can provide slightly improved
performance, and some subjective aesthetics.

```haskell
#!/usr/bin/env stack
{- stack
     --resolver lts-6.15
     --install-ghc
     runghc
     --package fsnotify-conduit
     --package conduit-combinators
 -}

import Conduit
import Data.Acquire (with)
import Data.Conduit.FSNotify
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dir <-
        case args of
            [dir] -> return dir
            _ -> error $ "Expected one argument (directory to watch)"
    with
        (acquireSourceFileChanges
            (setRelative False $ mkFileChangeSettings dir))
        ($$ mapM_C print)
```
