-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  {c} Mingli Yuan 2007
-- License     :  BSD-style {see the file libraries/base/LICENSE}
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main where

import System.Environment
import System.Console.GetOpt
import System.IO
import Data.Maybe ( fromMaybe )
import Control.Monad
import Text.ParserCombinators.Parsec

import Lang.While


data Flag = Verbose  | Version | Input String | Output String deriving Show

options :: [OptDescr Flag]
options = [
        Option ['v']     ["verbose"] (NoArg Verbose)         "chatty output on stderr",
        Option ['V','?'] ["version"]  (NoArg Version)         "show version number",
        Option ['o']     ["output"]    (ReqArg Output "FILE")  "output FILE",
        Option ['i']      ["input"]      (ReqArg Input "FILE")   "input FILE"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
          where header = "Usage: while [OPTION...] files..."


openInputFile :: [Flag] -> IO Handle
openInputFile flags = do {
        inputs <- filterInput flags;
        case (head inputs) of
            Input x -> openFile x ReadMode
    }

filterInput :: [Flag] -> IO [Flag]
filterInput flags = filterM f flags where
                              f flag= do {
                                case flag of
                                    Input x -> return True
                                    Output x -> return False
                                    Verbose -> return False
                                    Version -> return False
                              }

readInput :: Handle -> IO String
readInput handle = do {
               hGetContents handle;
    }

openProgramFile :: [String] -> IO Handle
openProgramFile filePaths = do {
        case filePaths of
            []    -> ioError (userError "program file is not ommitable.")
            x:[]  -> openFile x ReadMode
            x:(y:[]) -> ioError (userError "too many program files.")
    }

readProgram :: Handle -> IO String
readProgram handle = do {
               hGetContents handle;
    }

openOutputFile :: [Flag] -> IO Handle
openOutputFile flags = do {
        outputs <- filterOutput flags;
        case (head outputs) of
            Output x -> openFile x WriteMode
    }

filterOutput :: [Flag] -> IO [Flag]
filterOutput flags = filterM f flags where
                              f flag= do {
                                case flag of
                                    Input x -> return False
                                    Output x -> return True
                                    Verbose -> return False
                                    Version -> return False
                              }

run :: Show a => Parser a -> String -> Handle -> IO ()
run p code handle
    = case (parse p "" code) of
        Left err -> do { putStr "parse error at "
                        ; print err
                        }
        Right x -> hPrint handle x

main = do {
               args <- getArgs;
               opts <- compilerOpts args;
               inputFile <- openInputFile (fst opts);
               input <- readInput inputFile;
               progFile <- openProgramFile (snd opts);
               prog <- readProgram progFile;
               ouputFile <- openOutputFile (fst opts);
               run (program input) prog ouputFile;
               hClose inputFile;
               hClose progFile;
               hClose ouputFile
           }
