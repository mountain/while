-----------------------------------------------------------------------------
-- |
-- Module      :  Lang.While.Program
-- Copyright   :  (c) Mingli Yuan 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Lang.While.Program where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

import Lang.While.WhilyTree
import Lang.While.Store
import Lang.While.Lexer
import Lang.While.Runtime
import Lang.While.Command

program ::  String -> Parser WhilyTree
program input = do {
                    whiteSpace;
                    init <- readStatement input;
                    do {
                        whiteSpace;
                        cmd <- command init;
                        whiteSpace;
                        output <- writeStatement (execute (cmd, init));
                        whiteSpace;
                        optional eof;
                        return output
                    }
                  <|>
                    do {
                        whiteSpace;
                        output <- writeStatement init;
                        whiteSpace;
                        optional eof;
                        return output
                    }
                }

readStatement :: String -> Parser Bindings
readStatement input = do {
                                    reserved "read";
                                    whiteSpace;
                                    var <- identifier;
                                    whiteSpace;
                                    semi;
                                    return (assign var  (eval_whily input) intializeStore)
                                }
                              <?> "read command"

writeStatement :: Bindings -> Parser WhilyTree
writeStatement bnds =  do {
                                      reserved "write";
                                      whiteSpace;
                                      var <- identifier;
                                      whiteSpace;
                                      semi;
                                      return (lookupVar var bnds)
                                  }
                                <?> "write command"


