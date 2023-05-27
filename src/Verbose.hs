--
-- copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
--
-- this program is free software; you can redistribute it and/or modify
-- it under the terms of the gnu general public license as published by
-- the free software foundation; either version 2 of the license, or
-- (at your option) any later version.
--
-- this program is distributed in the hope that it will be useful,
-- but without any warranty; without even the implied warranty of
-- merchantability or fitness for a particular purpose.  see the
-- gnu general public license for more details.
--
-- you should have received a copy of the gnu general public license
-- along with this program; if not, write to the free software
-- foundation, inc., 59 temple place - suite 330, boston, ma 02111-1307, usa.
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