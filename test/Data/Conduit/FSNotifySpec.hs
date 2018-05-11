module Data.Conduit.FSNotifySpec where

import Test.Hspec (Spec, it, shouldBe)
import Data.Conduit (ZipSource (..), runConduitRes, (.|), yield)
import qualified Data.Conduit.List as CL
import Data.Conduit.FSNotify
import Control.Monad (forM_)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent (threadDelay)
import System.FilePath ((</>))
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad.IO.Class (liftIO)

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
        concurrently_
            (forM_ actions $ \(path, mcontents) -> do
                threadDelay 100000
                case mcontents of
                    Nothing -> removeFile (root </> path)
                    Just contents -> writeFile (root </> path) contents)
            (let src1 = sourceFileChanges (mkFileChangeSettings root)
                 src2 = mapM_ (yield . fst) actions
                 src = getZipSource ((,) <$> ZipSource src1 <*> ZipSource src2)
             in runConduitRes
                    $ src
                   .| CL.mapM_ (\(event, expected) -> liftIO $ eventPath event `shouldBe` expected))
