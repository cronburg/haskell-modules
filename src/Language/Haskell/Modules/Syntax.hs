module Language.Haskell.Modules.Syntax
  ( Name(..)
  , Module(..)
  , Package
  , defModule
  ) where

data Name = MN String | Fncn String
  deriving (Eq, Show)

data Module = Module
  { name    :: Name           -- name of this module
  , srcdir  :: FilePath
  , exports :: Maybe [Name]   -- list of names exported
  , imports :: [Name]         -- list of names imported
  , decls   :: [Name]
  , env     :: [Name]
  } deriving (Eq, Show)

type Package = [Module]

defModule :: Module
defModule = Module
  { name    = MN ""
  , srcdir  = ""
  , exports = Nothing
  , imports = []
  , decls   = []
  , env     = []
  }

