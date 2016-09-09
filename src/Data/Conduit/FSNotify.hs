{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Conduit.FSNotify
    ( -- * Conduit API
      sourceFileChanges
    , acquireSourceFileChanges
    , FileChangeSettings
    , mkFileChangeSettings

      -- * Setters
    , setWatchConfig
    , setRelative
    , setRecursive
    , setPredicate

      -- * Re-exports
    , FS.Event (..)
    , FS.eventTime
    , FS.eventPath
    , FS.WatchConfig (..)
    , FS.Debounce (..)
    ) where

import Control.Exception (assert)
import Data.Conduit
import Data.Acquire
import Control.Monad.Trans.Resource (MonadResource, release)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever)
import qualified System.FSNotify as FS
import System.Directory (canonicalizePath)
import Control.Concurrent.Chan
import Data.List (stripPrefix)
import System.FilePath (addTrailingPathSeparator)

-- | Settings for watching for file changes, to be passed in to
-- 'sourceFileChanges'. Should be created with 'mkFileChangeSettings'.
--
-- @since 0.1.0.0
data FileChangeSettings = FileChangeSettings
    { fcsDir :: !FilePath
    , fcsWatchConfig :: !FS.WatchConfig
    , fcsRelative :: !Bool
    , fcsRecursive :: !Bool
    , fcsPredicate :: !(FS.Event -> Bool)
    }

-- | Override the 'FS.WatchConfig' when creating the 'FS.WatchManager'.
--
-- Default: 'FS.defaultConfig'
--
-- @since 0.1.0.0
setWatchConfig :: FS.WatchConfig -> FileChangeSettings -> FileChangeSettings
setWatchConfig x fcs = fcs { fcsWatchConfig = x }

-- | Whether to provide paths relative to the root directory (@True@)
-- or absolute paths (@False@).
--
-- Default: 'True' (relative paths)
--
-- @since 0.1.0.0
setRelative :: Bool -> FileChangeSettings -> FileChangeSettings
setRelative x fcs = fcs { fcsRelative = x }

-- | Recursively watch a directory tree?
--
-- Default: 'True'
--
-- @since 0.1.0.0
setRecursive :: Bool -> FileChangeSettings -> FileChangeSettings
setRecursive x fcs = fcs { fcsRecursive = x }

-- | Predicate used to filter events.
--
-- Default: @const True@ (allow all events)
--
-- @since 0.1.0.0
setPredicate :: (FS.Event -> Bool) -> FileChangeSettings -> FileChangeSettings
setPredicate x fcs = fcs { fcsPredicate = x }

-- | Create a 'FileChangeSettings' from a directory to watch. Provides
-- defaults which can be overridden by the setter functions in this
-- module, such as 'setRelative'.
--
-- @since 0.1.0.0
mkFileChangeSettings :: FilePath -- ^ directory to watch
                     -> FileChangeSettings
mkFileChangeSettings dir = FileChangeSettings
    { fcsDir = dir
    , fcsWatchConfig = FS.defaultConfig
    , fcsRelative = True
    , fcsRecursive = True
    , fcsPredicate = const True
    }

-- | Watch for changes to a directory, and yield the file paths
-- downstream. Typical usage would be:
--
-- @
-- sourceFileChanges (setRelative False $ mkFileChangeSettings dir)
-- @
--
-- @since 0.1.0.0
sourceFileChanges :: MonadResource m
                  => FileChangeSettings
                  -> Producer m FS.Event
sourceFileChanges fcs = do
    (releaseKey, src) <- allocateAcquire $ acquireSourceFileChanges fcs
    src
    release releaseKey

-- | Same as 'sourceFileChanges', but returns the stream as an
-- 'Acquire' value, so that 'ResourceT' does not need to be used. See
-- the README.md for example usage.
--
-- @since 0.1.1.0
acquireSourceFileChanges
    :: MonadIO m
    => FileChangeSettings
    -> Acquire (ConduitM i FS.Event m ())
acquireSourceFileChanges FileChangeSettings {..} = do
    man <- mkAcquire (FS.startManagerConf fcsWatchConfig) FS.stopManager

    -- Get the absolute path of the root directory
    root' <- liftIO $ canonicalizePath fcsDir

    -- Create a channel for communication between two threads. Since
    -- file watch events come in asynchronously on separate threads,
    -- we want to fill up a channel with those events, and then below
    -- read the values off that channel.
    chan <- liftIO newChan

    -- Start watching a directory tree, accepting all events (const True).
    let watchChan = if fcsRecursive then FS.watchTreeChan else FS.watchDirChan

    _stop <- mkAcquire (watchChan man root' fcsPredicate chan) id

    return $ forever $ do
        event <- liftIO $ readChan chan

        if fcsRelative
            then do
                -- The complete file path of the event.
                let fp = FS.eventPath event

                -- Since we want the path relative to the directory root,
                -- strip off the root from the file path
                case stripPrefix (addTrailingPathSeparator root') fp of
                    Nothing -> assert False $ return ()
                    Just suffix
                        -- Ignore changes to the root directory itself
                        | null suffix -> return ()

                        -- Got a change to the file, write it to the channel
                        | otherwise -> yield $
                            case event of
                                FS.Added _ time -> FS.Added suffix time
                                FS.Modified _ time -> FS.Modified suffix time
                                FS.Removed _ time -> FS.Removed suffix time
            else yield event
