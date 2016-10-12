module Language.Haskell.Modules.Syntax where

data Name = MN String | Fncn String
  deriving (Eq, Show)

data Module = Module
  { name    :: Maybe Name     -- name of this module
  , exports :: Maybe [Name]   -- list of names exported
  , imports :: [Name]         -- list of names imported
  , env     :: [Name]         -- list of names in scope
  } deriving (Eq, Show)

