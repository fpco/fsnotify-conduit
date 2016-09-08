module Data.Conduit.FSNotifySpec where

import Test.Hspec (Spec, it, shouldBe)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.FSNotify
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent (threadDelay)
import System.FilePath ((</>))
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
    it "sourceFileChanges" $ withSystemTempDirectory "source-file-changes" $ \root -> do
        chan <- newChan
        let actions =
                [ ("foo", Just "hello")
                , ("bar", Just "world")
                , ("foo", Just "!")
                , ("bar", Nothing)
                , ("foo", Nothing)
                ]
        runConcurrently $
            Concurrently (runResourceT $ sourceFileChanges (mkFileChangeSettings root)
                                      $$ CL.mapM_ (liftIO . writeChan chan)) <|>
            Concurrently (forM_ actions $ \(path, mcontents) -> do
                threadDelay 100000
                case mcontents of
                    Nothing -> removeFile (root </> path)
                    Just contents -> writeFile (root </> path) contents) <|>
            Concurrently (forM_ actions $ \(expected, _) -> do
                event <- readChan chan
                eventPath event `shouldBe` expected)
