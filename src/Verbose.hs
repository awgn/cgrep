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

import Options ( Options(verbosity) )
import Reader ( OptionIO )


putStrLn1 :: String -> OptionIO ()
putStrLn1 xs = do
    n <- reader $ verbosity . snd
    when (n > 0) $ liftIO $ putStrLn xs
{-# INLINE putStrLn1 #-}

putStrLn2 :: String -> OptionIO ()
putStrLn2 xs = do
    n <- reader $ verbosity . snd
    when (n > 1) $ liftIO $ putStrLn xs
{-# INLINE putStrLn2 #-}

putStrLn3 :: String -> OptionIO ()
putStrLn3 xs = do
    n <- reader $ verbosity . snd
    when (n > 2) $ liftIO $ putStrLn xs
{-# INLINE putStrLn3 #-}
