module Language.Haskell.Modules.Parser
  ( parseModule
  , parsePackage
  , buildModuleEnv
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
import System.IO

import Language.Haskell.Modules.Syntax

------------------------------------------------------------------------------
-- Semantics

-- Get a list of the names introduced when another module imports the given
-- module:
getExports :: Module -> [Name]
getExports Module { exports = Nothing, decls = ds } = ds
getExports Module { exports = Just es} = es

-- Update a module's environment according to its imports and decls
importEnv :: Package -> Module -> Module
importEnv [] m = m { env = env m ++ decls m }
importEnv (m':ms') m
  | name m' `elem` imports m = importEnv ms' (m { env = env m ++ getExports m' })
  | otherwise = importEnv ms' m

buildModuleEnv :: Package -> Package
buildModuleEnv pkg = foldr ((:) . importEnv pkg) [] pkg
--  let bME [] = []
--      bME (m:ms) = (m { env = importEnv pkg m } : bME ms)
--  in bME pkg

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
maybeExports =
       (parens exportList >>= pure . Just)
  <||> (return Nothing)

parseImport =
       (reserved "import" >> reserved "qualified" >> identifier >>= pure . MN)
  <||> (reserved "import" >> identifier >>= pure . MN)

parseImports = many parseImport

-- Parse all modules in the given source
parsePackage :: FilePath -> FilePath -> IO (Either [ParseError] Package)
parsePackage srcdir ignore = do
  ms' <- listDirectory srcdir
  let ms = filter (ignore /=) ms'
  cs <- mapM (\f -> readFile $ srcdir ++ f) ms
  let es = map (\(fn,cont) -> PP.parse (parseModule srcdir) fn cont) $ zip ms cs
  if null $ lefts es
    then return $ Right $ rights es
    else return $ Left  $ lefts  es

nameDecl :: PS.Parser Name
nameDecl = do
  n <- identifier
  reservedOp "::"
  _ <- identifier
  pure $ Fncn n

nameDecls :: PS.Parser [Name]
nameDecls = many nameDecl

parseModule :: FilePath -> PS.Parser Module
parseModule srcdir = do
  reserved "module"
  n     <- identifier
  es    <- maybeExports
  reserved "where"
  is    <- parseImports
  ds    <- nameDecls
  return $ defModule
    { name    = MN n
    , srcdir  = srcdir
    , exports = es
    , imports = is
    , decls   = ds
    }

------------------------------------------------------------------------------
-- Or-Try Combinator (tries two parsers, one after the other)
(<||>) a b = try a <|> try b

------------------------------------------------------------------------------
-- Lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["::"]
  , reservedNames   = ["module", "qualified", "as", "where"]
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

