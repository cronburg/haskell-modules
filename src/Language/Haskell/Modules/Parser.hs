module Language.Haskell.Modules.Parser
  ( parseModule
  , parsePackage
  ) where

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
import Data.Either (lefts, rights)

import Language.Haskell.Modules.Syntax

------------------------------------------------------------------------------
-- Semantics
buildModuleEnv :: Package -> Package
buildModuleEnv pkg = pkg

-----------------------------------------------------------------------------
-- Parsers

moduleExport :: PS.Parser Name
moduleExport = 
       (reserved "qualified" >> reserved "module" >> identifier >>= pure . MN)
  <||> (reserved "module" >> identifier >>= pure . MN)

exportItem :: PS.Parser Name
exportItem = do
       moduleExport
  <||> (identifier >>= pure . Fncn)
exportList :: PS.Parser [Name]
exportList = sepBy exportItem (reservedOp ",")

maybeExports :: PS.Parser (Maybe [Name])
maybeExports = (do
    reservedOp "("
    es <- exportList
    reservedOp ")"
    return $ Just es
    )
  <||> (return Nothing)

parseImports = return []

-- Parse all modules in the given source
parsePackage :: FilePath -> IO (Either [ParseError] Package)
parsePackage srcdir = do
  ms <- listDirectory srcdir
  cs <- mapM (\f -> readFile $ srcdir ++ f) ms
  let es = map (\(fn,cont) -> PP.parse (parseModule srcdir) fn cont) $ zip ms cs
  if length (lefts es) == 0
    then return $ Right $ rights es
    else return $ Left  $ lefts  es

parseModule :: FilePath -> PS.Parser Module
parseModule srcdir = do
  reserved "module"
  n    <- identifier
  es   <- maybeExports
  reserved "where"
  is   <- parseImports
  return $ defModule
    { name    = MN n
    , srcdir  = srcdir
    , exports = es
    , imports = is
    }

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["::"]
  , reservedNames   = ["module", "qualified", "as", "Int", "where"]
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

