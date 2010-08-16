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

module Lang.While.Command where

import Text.ParserCombinators.Parsec

import Lang.While.WhilyTree
import Lang.While.Store
import Lang.While.Runtime
import Lang.While.Lexer
import Lang.While.Expression

command :: Bindings -> Parser Command
command bnds = do {
                             compondCommand bnds;
                          }
                       <?>
                         "command"

compondCommand :: Bindings -> Parser Command
compondCommand bnds = do {
                            cmds <- manyCommand  bnds;
                            return (cmdCompond cmds)
                        }
                       <?>
                         "compond command"

manyCommand :: Bindings -> Parser [Command]
manyCommand bnds = scan bnds id
                                 where
                                      scan bindings f = do{
                                                        cmd <- simpleCommand bindings;
                                                        scan (execute (cmd, bindings)) (\tail -> f (cmd:tail))
                                                    }
                                                <|> return (f [])

simpleCommand :: Bindings -> Parser Command
simpleCommand bnds = noopCommand bnds
                       <|>
                         assignCommand bnds
                       <|>
                         doWhileCommand bnds
                       <?>
                         "simple command"

doWhileCommand :: Bindings -> Parser Command
doWhileCommand bnds = do {
           reserved "while";
           whiteSpace;
           judge <- expr;
           whiteSpace;
           reserved "do";
           whiteSpace;
           char '[';
           whiteSpace;
           cmd <- command bnds;
           whiteSpace;
           char ']';
           whiteSpace;
           semi;
           whiteSpace;
           return (cmdDoWhile judge cmd)
       }
       <?> "do-while command"

assignCommand :: Bindings -> Parser Command
assignCommand bnds = do {
           var <- identifier;
           whiteSpace;
           char '=';
           whiteSpace;
           evalExpr <- expr;
           whiteSpace;
           semi;
           whiteSpace;
           return (cmdAssign var evalExpr)
       }
       <?> "assignment"

noopCommand :: Bindings -> Parser Command
noopCommand bnds = do {
           semi;
           return cmdNoop
       }
       <?> "noop"


