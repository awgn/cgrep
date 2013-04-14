module CGrep.Output where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Options

data Output = StrictOutput FilePath Int  C.ByteString [String] |
              LazyOutput   FilePath Int LC.ByteString [String] 
                deriving (Show,Eq)


showOutput :: Options -> Output -> String
showOutput Options { no_filename = False, no_linenumber = False } (StrictOutput f n l _) = f ++ ":" ++ show n ++ ":" ++ C.unpack l
showOutput Options { no_filename = False, no_linenumber = True  } (StrictOutput f _ l _) = f ++ ":" ++ C.unpack l
showOutput Options { no_filename = True , no_linenumber = False } (StrictOutput _ n l _) = show n ++ ":" ++ C.unpack l
showOutput Options { no_filename = True , no_linenumber = True  } (StrictOutput _ _ l _) = C.unpack l


showOutput Options { no_filename = False, no_linenumber = False } (LazyOutput f n l _) = f ++ ":" ++ show n ++ ":" ++ LC.unpack l
showOutput Options { no_filename = False, no_linenumber = True  } (LazyOutput f _ l _) = f ++ ":" ++ LC.unpack l
showOutput Options { no_filename = True , no_linenumber = False } (LazyOutput _ n l _) = show n ++ ":" ++ LC.unpack l
showOutput Options { no_filename = True , no_linenumber = True  } (LazyOutput _ _ l _) = LC.unpack l
