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

module Verbose where

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad ( when )

import Options ( Options(verbose) )
import Reader ( ReaderIO, Env(..) )

import qualified Data.ByteString as C (hPutStr, hPut)
import GHC.IO.Handle ( Handle )
import System.IO ( Handle, hPutStrLn, hPutStr )
import Data.String ( IsString )

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as T


class (IsString a) => PutStr a where
    putStringLn :: Handle -> a -> IO ()
    putString :: Handle -> a -> IO ()

instance PutStr String where
    putStringLn = hPutStrLn
    putString = hPutStr

instance PutStr C.ByteString where
    putStringLn = C.hPutStrLn
    putString = C.hPutStr

instance PutStr T.Text where
    putStringLn = T.hPutStrLn
    putString = T.hPutStr


putMsgLnVerbose :: (PutStr a) => Int -> Handle -> a -> ReaderIO ()
putMsgLnVerbose l h xs = do
    n <- reader $ verbose . opt
    when (n >= l) $
        liftIO $ putStringLn h xs
{-# INLINE putMsgLnVerbose #-}


putMsgLn :: (PutStr a, MonadIO m) => Handle -> a -> m ()
putMsgLn h xs =
    liftIO $ putStringLn h xs
{-# INLINE putMsgLn #-}