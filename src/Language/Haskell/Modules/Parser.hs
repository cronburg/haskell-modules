module Language.Haskell.Modules.Parser
  ( parseModule, parseModules ) 
  where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import qualified Text.Parsec.Expr as PE
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos      (newPos)
import Data.Char        -- Provides isDigit and isSpace functions
import System.Directory
import Control.Monad (zipWithM)
import Language.Haskell.Modules.Syntax

------------------------------------------------------------------------------
-- Parsers

-- Parse all modules in the given source
parseModules :: FilePath -> IO [Either ParseError Module]
parseModules srcdir = do
  ms <- listDirectory srcdir
  cs <- mapM (\f -> readFile $ srcdir ++ f) ms
  return $ map (\(fn,cont) -> PP.parse (parseModule srcdir) fn cont) $ zip ms cs

parseModule :: FilePath -> PS.Parser Module
parseModule srcdir = do
  reserved "module"
  name <- identifier
  return Module
    { name    = Just $ MN name
    , exports = Nothing
    , imports = []
    , env     = []
    }

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["::"]
  , reservedNames   = ["module", "qualified", "as", "Int"]
  }

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
operator      = PT.operator    lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
integer       = PT.integer     lexer
natural       = PT.natural     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer

expr = PE.buildExpressionParser table term
       <?> "expression"
term = natural
       <?> "simple expression"
table = [ [prefix "-" negate, prefix "+" id ] ]
prefix   name fun = PE.Prefix $ reservedOp name >> return fun

