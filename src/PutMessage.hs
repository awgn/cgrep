--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (reader)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (Handle)
import Options (Options (..))
import Reader (Env (..), ReaderIO)
import System.IO (hPutStr, hPutStrLn)

class (IsString a) => PutStr a where
    putStringLn :: Handle -> a -> IO ()
    putString :: Handle -> a -> IO ()

instance PutStr String where
    putStringLn = hPutStrLn
    {-# INLINE putStringLn #-}
    putString = hPutStr
    {-# INLINE putString #-}

instance PutStr T.Text where
    putStringLn = TIO.hPutStrLn
    {-# INLINE putStringLn #-}
    putString = TIO.hPutStr
    {-# INLINE putString #-}

putMessageLnVerb :: (PutStr a) => Int -> Handle -> a -> ReaderIO ()
putMessageLnVerb l h xs = do
    n <- reader $ debug . opt
    when (n >= l) $
        liftIO $
            putStringLn h xs
{-# INLINE putMessageLnVerb #-}

putMessageLn :: (PutStr a, MonadIO m) => Handle -> a -> m ()
putMessageLn h xs =
    liftIO $ putStringLn h xs
{-# INLINE putMessageLn #-}
