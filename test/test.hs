--
-- Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam.
-- Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
--

{-
      Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus.
      Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui.
      Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque
      tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
 -}

{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here

compute :: Int -> String -> Int
compute x label =
    let base = 42
     in if x > 0 then x + base else 0

main :: IO ()
main = do
       let res = compute 10 "default"
       putStrLn "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\""
       putStrLn [here| Sed etiam a suspendisse. "Aliquam nulla erat risus." |]
       print res
