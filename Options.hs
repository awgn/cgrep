{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import Data.Data

data Options = Options 
               {
                -- Pattern:
                file    :: String,
                word    :: Bool,
                regex   :: Bool,
                icase   :: Bool,
                -- Context:
                code    :: Bool,
                comment :: Bool,
                string  :: Bool,
                others  :: [String],
                -- General:
                jobs      :: Int,
                multiline :: Bool,
                recursive :: Bool

               } deriving (Data, Typeable, Show)

