{-# LANGUAGE DeriveDataTypeable #-}

module CGrep.Options where

import Data.Data

data Options = Options 
               {
                -- Pattern:
                file    :: String,
                word    :: Bool,
                regex   :: Bool,
                ignore_case :: Bool,
                -- Context:
                code    :: Bool,
                comment :: Bool,
                string  :: Bool,
                tokens  :: [String],
                -- Output:
                no_filename     :: Bool,
                no_linenumber   :: Bool,
                -- General:
                jobs      :: Int,
                multiline :: Bool,
                recursive :: Bool,
                invert_match :: Bool,
                boyer_moore :: Bool,

                debug  :: Bool,
                others :: [String]
               } deriving (Data, Typeable, Show)

