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

import Control.Monad

putStrLevel1 :: Int -> String -> IO ()
putStrLevel1 n xs = when (n > 0) $ putStrLn xs

putStrLevel2 :: Int -> String -> IO ()
putStrLevel2 n xs = when (n > 1) $ putStrLn xs

putStrLevel3 :: Int -> String -> IO ()
putStrLevel3 n xs = when (n > 2) $ putStrLn xs
