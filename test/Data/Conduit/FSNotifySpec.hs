module Data.Conduit.FSNotifySpec where

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Data.Conduit.FSNotify
import Control.Concurrent.Chan
import Control.Applicative
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import System.FilePath
import System.Directory
import System.IO.Temp
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
            Concurrently (runResourceT $ sourceFileChanges root $$ CL.mapM_ (liftIO . writeChan chan)) <|>
            Concurrently (forM_ actions $ \(path, mcontents) -> do
                threadDelay 100000
                case mcontents of
                    Nothing -> removeFile (root </> path)
                    Just contents -> writeFile (root </> path) contents) <|>
            Concurrently (forM_ actions $ \(expected, _) -> do
                actual <- readChan chan
                actual `shouldBe` expected)
