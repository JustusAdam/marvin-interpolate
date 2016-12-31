module Util where


import Language.Haskell.TH.Quote


mqq :: QuasiQuoter
mqq = QuasiQuoter 
    { quotePat = error "Interpolation cannot occur in pattern"
    , quoteType = error "Interpolation cannot occur in type"
    , quoteDec = error "Interpolation cannot occur in declaration"
    , quoteExp = error "Interpolation not defined, please contact library author :D"
    }
