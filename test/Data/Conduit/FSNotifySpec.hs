module Data.Conduit.FSNotifySpec where

import Test.Hspec (Spec, it, shouldBe)
import Data.Conduit (ZipSource (..), runConduit, (.|), yield, await)
import qualified Data.Conduit.List as CL
import Data.Conduit.FSNotify
import Control.Monad (forM_)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent (threadDelay)
import System.FilePath ((</>))
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.IO
import qualified Data.Acquire

spec :: Spec
spec = do
    it "sourceFileChanges" $ withSystemTempDirectory "source-file-changes" $ \root -> do
        let actions =
                [ ("foo", Just "hello")
                , ("bar", Just "world")
                , ("foo", Just "!")
                , ("bar", Nothing)
                , ("foo", Nothing)
                ]
            go (path, mcontents) = do
                liftIO $ threadDelay 100000
                liftIO $ hPrint stderr (path, mcontents)
                liftIO $ case mcontents of
                    Nothing -> removeFile (root </> path)
                    Just contents -> writeFile (root </> path) contents
                mnext <- await
                liftIO $ hPrint stderr mnext
                case mnext of
                    Nothing -> error "Unexpected empty"
                    Just event -> liftIO $ eventPath event `shouldBe` path
        Data.Acquire.with (acquireSourceFileChanges $ mkFileChangeSettings root) $ \src ->
            runConduit $ src .| mapM_ go actions
        return () :: IO () -- force the type
