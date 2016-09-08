{-# LANGUAGE RankNTypes #-}
module Data.Conduit.FSNotify where

import Data.Conduit
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import qualified System.FSNotify as FS
import System.Directory (canonicalizePath)
import Control.Concurrent.Chan
import Data.List (stripPrefix)
import System.FilePath (addTrailingPathSeparator)

-- | Watch for changes to a directory, and yield the file paths
-- downstream.
--
-- @since 0.1.0.0
sourceFileChanges :: MonadResource m
                  => FilePath
                  -> Producer m FilePath
sourceFileChanges root =
  -- The bracketP function allows us to safely allocate some resource
  -- and guarantee it will be cleaned up. In our case, we are calling
  -- startManager to allocate a file watching manager, and stopManager
  -- to clean it up. These functions will under the surface tie in to
  -- OS-specific file watch mechanisms, such as inotify on Linux.
  bracketP FS.startManager FS.stopManager $ \man -> do
    -- Get the absolute path of the root directory
    root' <- liftIO $ canonicalizePath root

    -- Create a channel for communication between two threads. Since
    -- file watch events come in asynchronously on separate threads,
    -- we want to fill up a channel with those events, and then below
    -- read the values off that channel.
    chan <- liftIO newChan

    -- Start watching a directory tree, accepting all events (const True).
    bracketP (FS.watchTreeChan man root' (const True) chan) id $ const $ forever $ do
        event <- liftIO $ readChan chan

        -- The complete file path of the event.
        let fp = FS.eventPath event

        -- Since we want the path relative to the directory root,
        -- strip off the root from the file path
        case stripPrefix (addTrailingPathSeparator root') fp of
            Nothing -> error $ "sourceFileChanges: prefix not found " ++ show (root', fp)
            Just suffix
                -- Ignore changes to the root directory itself
                | null suffix -> return ()

                -- Got a change to the file, write it to the channel
                | otherwise -> yield suffix
