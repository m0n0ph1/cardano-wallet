module Test.Integration.Framework.Profile where

import Prelude

import Control.Concurrent.STM
    ( atomically )
import Control.Concurrent.STM.TVar
    ( TVar, modifyTVar', newTVarIO, readTVarIO )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( MonadMask, finally )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.List
    ( sortOn )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Time.Clock.POSIX
    ( getPOSIXTime )
import Fmt
    ( blockListF', build, fixedF, fmt )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.Map.Strict as Map

{-# NOINLINE profileVar #-}
profileVar :: TVar (Map String [Double])
profileVar = unsafePerformIO $ newTVarIO Map.empty

bracketProfileIO :: (MonadIO m, MonadMask m) => String -> m a -> m a
bracketProfileIO label action = do
    t0 <- liftIO getPOSIXTime
    action `finally` (do
        t1 <- liftIO getPOSIXTime
        void $ liftIO $ atomically $ modifyTVar' profileVar
            $ Map.insertWith (++) label [fromRational $ toRational $ t1-t0])

logTestProfile :: IO ()
logTestProfile = do
    m <- readTVarIO profileVar
    putStrLn $ fmt $ blockListF' "" f (sortOn (Down . sum . snd) $ Map.toList m)
  where
    f (label, xs) = fixedF 2 (sum xs) <> "s, N=" <> build (length xs)
        <> " - " <> build label

-- f (label, xs) = listF' (fixedF 2) xs <> "s, N=" <> build (length xs)
--    avg xs = sum xs / fromIntegral (length xs)
