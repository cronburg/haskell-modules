ex00_exp =
  [ defModule'
      { name    = MN "M"
      , exports = Just [Fncn "x", Fncn "y", Fncn "z"]
      , imports = []
      , decls   = [Fncn "x", Fncn "y", Fncn "z"]
      , env     = [Fncn "x", Fncn "y", Fncn "z"]
      }
  , defModule'
      { name = MN "N"
      , exports = Just [Fncn "x", Fncn "y", Fncn "z"]
      , imports = [MN "M"]
      , decls   = []
      , env     = [Fncn "x", Fncn "y", Fncn "z"]
      }
  ]

