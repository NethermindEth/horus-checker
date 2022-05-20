{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Bits    
import Data.Text (Text)
import Control.Monad.Except
import Text.Printf
import GHC.Generics
import Data.Map
import Numeric (readHex)
import Data.Aeson (FromJSON, decode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Types
import Data.Maybe (fromMaybe)

import Horus.Instruction 

data CairoProgram = CairoProgram {
    attributes :: [String],
    builtins :: [String],
    code :: [Integer],
    hints :: Map String String,
    mainScope :: String,
    prime :: Integer 
} deriving (Generic, Show)

instance FromJSON CairoProgram where
    parseJSON (Object v) = CairoProgram
        <$> v .: "attributes"
        <*> v .: "builtins"
        <*> ((hexToInteger <$>) <$> v .: "data")
        <*> v .: "hints"
        <*> v .: "main_scope"
        <*> (hexToInteger <$> v .: "prime")
        where
            hexToInteger :: [Char] -> Integer
            hexToInteger hex = fst $ head $ readHex $ tail $ tail $ hex 

main :: IO ()
main = do
    args <- getArgs
    jsonString <- readFile $ head args
    let json = BS.pack jsonString
    fromMaybe (print "Cannot parse the file") $ do
        program <- decode json
        let firstInstr = head $ code program
        let imm = head $ tail (code program)
        Just $ let
            instr = decodeCairoInstruction firstInstr (Just imm) 
            in either print print instr

