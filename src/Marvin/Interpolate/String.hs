{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.String where


import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as L


class ShowS a where
    showS :: a -> String
    showListS :: [a] -> String
    showListS l = "[" <> intercalate ", " (map showS l) <> "]"

instance ShowS Char where
    showS = show
    showListT = id

instance ShowS Text where
    showS = T.unpack

instance ShowS L.Text where
    showS = L.unpack

instance {-# OVERLAPPABLE #-} Show a => ShowS a where
    showS = show


iS :: QuasiQuoter
iS = mqq { quoteExp = return . interpolateInto (VarE 'show) }
