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

module Lang.While.Lexer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle, haskellDef, reservedNames, reservedOpNames )

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
    (haskellDef {
                     reservedNames = ["while", "do", "read", "write"],
                     reservedOpNames = ["cons","hd","tl","=?"]
                }
    )

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
comma       = P.comma lexer
identifier = P.identifier lexer
stringLiteral = P.stringLiteral lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

