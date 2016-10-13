module Language.Haskell.Modules.Syntax
  ( Name(..)
  , Module(..)
  , Package
  , defModule
  ) where
import Data.Set as S (Set(..))

data Name = MN String | Fncn String
  deriving (Eq, Show)

-- Qualified or unqualified imports
data Import = Qual String | Unqual String

data Module = Module
  { name    :: Name           -- name of this module
  , srcdir  :: FilePath
  , exports :: Maybe [Name]   -- list of names exported
  , imports :: [Import]       -- list of names imported
  , decls   :: [Name]
  , env     :: [Name]
  } deriving (Show, Eq)

--instance Eq Module where
--  m1 == m2 = name m1 == name m2

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

