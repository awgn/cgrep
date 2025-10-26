--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

module PutMessage (
    PutStr (..),
    putMessageLnVerb,
    putMessageLn,
    putMessage,
) where

import Control.Concurrent (MVar)
import Control.Concurrent.MVar (withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (reader)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import GHC.IO.Handle (Handle)
import Options (Options (..))
import Reader (Env (..), ReaderIO)
import System.IO (hPutStr, hPutStrLn)

class (IsString a) => PutStr a where
    putMsgLnLock :: MVar () -> Handle -> a -> IO ()
    putMsgLock :: MVar () -> Handle -> a -> IO ()

instance PutStr String where
    putMsgLnLock lock h msg = withMVar lock (\_ -> hPutStrLn h msg)
    {-# INLINE putMsgLnLock #-}
    putMsgLock lock h msg = withMVar lock (\_ -> hPutStr h msg)
    {-# INLINE putMsgLock #-}

instance PutStr T.Text where
    putMsgLnLock lock h msg = withMVar lock (\_ -> TIO.hPutStrLn h msg)
    {-# INLINE putMsgLnLock #-}
    putMsgLock lock h msg = withMVar lock (\_ -> TIO.hPutStr h msg)
    {-# INLINE putMsgLock #-}

instance PutStr TL.Text where
    putMsgLnLock lock h msg = withMVar lock (\_ -> TLIO.hPutStrLn h msg)
    {-# INLINE putMsgLnLock #-}
    putMsgLock lock h msg = withMVar lock (\_ -> TLIO.hPutStr h msg)
    {-# INLINE putMsgLock #-}

putMessageLnVerb :: (PutStr a) => Int -> MVar () -> Handle -> a -> ReaderIO ()
putMessageLnVerb lvl lock h xs = do
    n <- reader $ debug . opt
    when (n >= lvl) $
        liftIO $
            putMsgLnLock lock h xs
{-# INLINE putMessageLnVerb #-}

putMessageLn :: (PutStr a, MonadIO m) => MVar () -> Handle -> a -> m ()
putMessageLn lock h xs =
    liftIO $ putMsgLnLock lock h xs
{-# INLINE putMessageLn #-}

putMessage :: (PutStr a, MonadIO m) => MVar () -> Handle -> a -> m ()
putMessage lock h xs =
    liftIO $ putMsgLock lock h xs
{-# INLINE putMessage #-}
