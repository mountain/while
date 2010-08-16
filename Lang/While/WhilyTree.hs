-----------------------------------------------------------------------------
-- |
-- Module      :  Lang.While.WhilyTree
-- Copyright   :  {c} Mingli Yuan 2007
-- License     :  BSD-style {see the file libraries/base/LICENSE}
--
-- Maintainer  :  me@mingli-yuan.info
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Lang.While.WhilyTree (
    WhilyTree,
    nil, mk, unary, cons, hd, tl, eq,
    tnot, tand, tor, boolize,
    whily_tree, eval_whily
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Data.Int
import Data.HashTable

import Lang.While.Lexer

---------------------------------------------
-- Structure part
---------------------------------------------

data WhilyTree = Nil | Elem String | Unary WhilyTree | Binary WhilyTree WhilyTree
                                                                                        deriving Eq

nil :: WhilyTree
nil = Nil

mk :: String -> WhilyTree
mk x = Elem x

unary :: WhilyTree -> WhilyTree
unary x = Unary x

cons :: WhilyTree -> WhilyTree -> WhilyTree
cons x y = Binary x y

hd :: WhilyTree -> WhilyTree
hd tree =
    case tree of
      Nil        -> Nil
      Elem x     -> Elem x
      Unary y    -> Unary y
      Binary x y -> x

tl :: WhilyTree -> WhilyTree
tl tree =
    case tree of
      Nil        -> Nil
      Elem x     -> Elem x
      Unary y    -> Unary y
      Binary x y -> y

eq :: WhilyTree -> WhilyTree -> WhilyTree
eq left right =
    case left of
      Nil        -> eqNil right
                        where eqNil a =
                                    case a of
                                      Nil        -> Binary Nil Nil
                                      Elem e     -> Nil
                                      Unary r    -> Nil
                                      Binary s t -> Nil
      Elem x     -> eqElem x right
                        where eqElem a b =
                                    case b of
                                      Nil        -> Nil
                                      Elem e     -> tbool (a == e)
                                      Unary r    -> Nil
                                      Binary s t -> Nil
      Unary y    -> eqUnary y right
                        where eqUnary a b =
                                    case b of
                                      Nil        -> Nil
                                      Elem e     -> Nil
                                      Unary r    -> eq a r
                                      Binary s t -> Nil
      Binary x y -> eqBinary x y right
                        where eqBinary a b c =
                                    case c of
                                      Nil        -> Nil
                                      Elem e     -> Nil
                                      Unary r    -> Nil
                                      Binary s t -> tand (eq a s) (eq b t)

tbool :: Bool -> WhilyTree
tbool False = Nil
tbool True = Binary Nil Nil

tnot :: WhilyTree -> WhilyTree
tnot x =
    case x of
      Nil        -> Binary Nil Nil
      Elem e     -> Nil
      Unary r    -> Nil
      Binary s t -> Nil

tand :: WhilyTree -> WhilyTree -> WhilyTree
tand left right =
    case left of
      Nil        -> Nil
      Elem x     -> tandElse right
                        where tandElse x =
                                    case x of
                                      Nil        -> Nil
                                      Elem e     -> Binary Nil Nil
                                      Unary r    -> Binary Nil Nil
                                      Binary s t -> Binary Nil Nil
      Unary y    -> tandElse right
                        where tandElse x =
                                    case x of
                                      Nil        -> Nil
                                      Elem e     -> Binary Nil Nil
                                      Unary r    -> Binary Nil Nil
                                      Binary s t -> Binary Nil Nil
      Binary x y -> tandElse right
                        where tandElse x =
                                    case x of
                                      Nil        -> Nil
                                      Elem e     -> Binary Nil Nil
                                      Unary r    -> Binary Nil Nil
                                      Binary s t -> Binary Nil Nil

tor :: WhilyTree -> WhilyTree -> WhilyTree
tor left right = tnot(tand (tnot left) (tnot right))

boolize :: WhilyTree -> Bool
boolize tree = not(tree == Nil)

streamS :: WhilyTree -> ShowS
streamS tree =
    case tree of
      Nil           -> showString "{}"
      Elem x     -> showString ("'" ++ x ++ "'")
      Unary y    -> showString ("{" ++ (show y) ++ "}")
      Binary x y -> showString ("{" ++ (show x) ++ ", " ++ (show y) ++ "}")

instance Show WhilyTree where
    show tree = streamS tree ""

hashWhilyTree :: WhilyTree -> Int32
hashWhilyTree tree =
    case tree of
      Nil        -> hashInt 0
      Elem x     -> hashString x
      Unary y    -> hashInt (fromIntegral (hashWhilyTree y))
      Binary x y -> hashInt (fromIntegral ((hashWhilyTree x) + (hashWhilyTree y)))

---------------------------------------------
-- Parser part
---------------------------------------------

element :: Parser String
element = many1 alphaNum

whily_tree :: Parser WhilyTree
whily_tree = do {
                  char '{';
                  whiteSpace;
                  do {
                      left <- whily_tree;
                      whiteSpace;
                      do {
                          comma;
                          whiteSpace;
                          do {
                                   char '\'';
                                   right <- element;
                                   char '\'';
                                   whiteSpace;
                                  char '}';
                                  whiteSpace;
                                  optional eof;
                                  return (cons left (mk right))
                          }
                          <|>
                          do {
                                  right <- whily_tree;
                                  whiteSpace;
                                  char '}';
                                  whiteSpace;
                                  optional eof;
                                  return (cons left right)
                          }
                      }
                      <|>
                      do {
                          char '}';
                          whiteSpace;
                          optional eof;
                          return (unary left)
                      }
                  }
                  <|>
                  do {
                      char '\'';
                      left <- element;
                      char '\'';
                      whiteSpace;
                      do {
                          comma;
                          whiteSpace;
                          do {
                                  char '\'';
                                  right <- element;
                                  char '\'';
                                  whiteSpace;
                                  char '}';
                                  whiteSpace;
                                  optional eof;
                                  return (cons (mk left) (mk right))
                          }
                          <|>
                          do {
                                  right <- whily_tree;
                                  whiteSpace;
                                  char '}';
                                  whiteSpace;
                                  optional eof;
                                  return (cons (mk left) right)
                          }
                      }
                      <|>
                      do {
                          char '}';
                          whiteSpace;
                          optional eof;
                          return (unary (mk left))
                      }
                  }
                  <|>
                  do {
                      char '}';
                      whiteSpace;
                      optional eof;
                      return Nil
                  }
              }
             <?> "while tree"

eval_whily :: String -> WhilyTree
eval_whily input
    = case (parse whily_tree "" input) of
           Left err -> nil
           Right x -> x

