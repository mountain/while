-----------------------------------------------------------------------------
-- |
-- Module      :  Lang.While.Runtime
-- Copyright   :  {c} Mingli Yuan 2007
-- License     :  BSD-style {see the file libraries/base/LICENSE}
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Lang.While.Runtime where

import Prelude

import Lang.While.WhilyTree
import Lang.While.Store

----------------------------------------------------------------
-- Expression related functions
----------------------------------------------------------------
data Expression = Const WhilyTree | Var String | Head Expression | Tail Expression
                           | Cons Expression Expression | Equals Expression Expression deriving Eq

exprConst :: WhilyTree -> Expression
exprConst tree = Const tree

exprVariable :: String -> Expression
exprVariable str = Var str

exprHead :: Expression -> Expression
exprHead expression = Head expression

exprTail :: Expression -> Expression
exprTail expression = Tail expression

exprConstruct :: Expression -> Expression -> Expression
exprConstruct expr1 expr2 = Cons expr1 expr2

exprEquals :: Expression -> Expression -> Expression
exprEquals expr1 expr2 = Equals expr1 expr2

streamS :: Expression -> ShowS
streamS expression =
    case expression of
      Const tree -> showString (show tree)
      Var var     -> showString var
      Head x      -> showString ("( hd " ++ (show x) ++ " )")
      Tail x        -> showString ("( tl " ++ (show x) ++ " )")
      Cons x y   -> showString ("( cons " ++ (show x) ++ " " ++ (show y) ++ " )")
      Equals x y -> showString ("( =? " ++ (show x) ++ " " ++ (show y) ++ " )")

instance Show Expression where
    show expression = streamS expression ""

----------------------------------------------------------------
-- Evaluation related functions
----------------------------------------------------------------
type Evaluation = (Expression, Bindings)

evaluate :: Evaluation -> WhilyTree
evaluate eval = 
    case (fst eval) of
      Const tree -> tree
      Var var     -> lookupVar var (snd eval)
      Head x      -> hd (evaluate (x, (snd eval)))
      Tail x        -> tl (evaluate (x, (snd eval)))
      Cons x y   -> cons (evaluate (x, (snd eval))) (evaluate (y, (snd eval)))
      Equals x y -> eq (evaluate (x, (snd eval)))  (evaluate (y, (snd eval)))

----------------------------------------------------------------
-- Command related functions
----------------------------------------------------------------
data Command = Noop | Assign String Expression | DoWhile Expression Command
                        | Compond [Command] deriving Eq

cmdNoop :: Command
cmdNoop = Noop

cmdAssign :: String -> Expression -> Command
cmdAssign var expression = Assign var expression

cmdDoWhile :: Expression -> Command -> Command
cmdDoWhile expression cmd = DoWhile expression cmd

cmdCompond :: [Command] -> Command
cmdCompond cmds = Compond cmds

----------------------------------------------------------------
-- Execution related functions
----------------------------------------------------------------
type Execution = (Command, Bindings)

execute :: Execution -> Bindings
execute execution =
                case (fst execution) of
                    Noop                            -> noopCmd (snd execution)
                    Assign var expression     -> assignCmd var expression (snd execution)
                    DoWhile expression cmd -> doWhileCmd expression cmd (snd execution)
                    Compond cmds              -> compondCmd cmds (snd execution)

compondCmd :: [Command] -> Bindings -> Bindings
compondCmd [] bnds = bnds
compondCmd (x:xs) bnds = compondCmd xs (execute (x, bnds))

noopCmd :: Bindings -> Bindings
noopCmd = id

assignCmd :: String -> Expression -> Bindings -> Bindings
assignCmd var expression init = assign var (evaluate (expression, init)) init

doWhileCmd :: Expression -> Command -> Bindings -> Bindings
doWhileCmd expression cmd init =
                    snd $ last (expand expression cmd init)

expandOnce :: Expression -> Command -> Execution -> Execution
expandOnce expression cmd execution =
           case (boolize $ evaluate (expression, snd execution)) of
               True -> (cmd, execute execution)
               False -> (cmdNoop, execute execution)

expand :: Expression -> Command -> Bindings -> [Execution]
expand expression cmd bnds = 
           whenStop (iterate f  (cmdNoop, bnds))
           where
               f = expandOnce expression cmd

whenStop :: [Execution] -> [Execution]
whenStop [] = []
whenStop (init:[]) = [init]
whenStop (init:executions) = init:(takeWhile ((/= Noop) . fst)  executions)


