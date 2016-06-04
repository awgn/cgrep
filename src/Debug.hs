--
-- copyright (c) 2013 bonelli nicola <bonelli@antifork.org>
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


module Debug where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad

import Options
import Reader


putStrLevel1 :: String -> OptionT IO ()
putStrLevel1 xs = do
    n <- reader $ debug . snd
    when (n > 0) $ liftIO $ putStrLn xs

putStrLevel2 :: String -> OptionT IO ()
putStrLevel2 xs = do
    n <- reader $ debug . snd
    when (n > 1) $ liftIO $ putStrLn xs

putStrLevel3 :: String -> OptionT IO ()
putStrLevel3 xs = do
    n <- reader $ debug . snd
    when (n > 2) $ liftIO $ putStrLn xs
