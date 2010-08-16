-----------------------------------------------------------------------------
-- |
-- Module      :  Lang.While.Expression
-- Copyright   :  (c) Mingli Yuan 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Lang.While.Expression where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Lang.While.WhilyTree
import Lang.While.Lexer
import Lang.While.Store
import Lang.While.Runtime

expr :: Parser Expression
expr = do {
           op <- reservedOp "cons";
           whiteSpace;
           left <- factor;
           whiteSpace;
           right <- factor;
           return (exprConstruct left right)
       }
       <|>
       do {
           op <- reservedOp "=?";
           whiteSpace;
           left <- factor;
           whiteSpace;
           right <- factor;
           return (exprEquals left right)
       }
       <|>
       do {
           op <- reservedOp "hd";
           whiteSpace;
           num <- factor;
           return (exprHead num)
       }
       <|>
       do {
           op <- reservedOp "tl";
           whiteSpace;
           num <- factor;
           return (exprTail num)
       }
       <|>
       factor
       <?> "expr"

factor :: Parser Expression
factor =  parens expr
       <|> constant
       <|> variable
       <?> "factor"

variable :: Parser Expression
variable = do {
                      var <- identifier;
                      return (exprVariable var)
               }
               <?> "variable"

constant :: Parser Expression
constant = do {
                      tree <- whily_tree;
                      return (exprConst tree)
               }
               <?> "constant"

