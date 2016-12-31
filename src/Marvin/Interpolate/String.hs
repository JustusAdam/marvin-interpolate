{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.String where


import           Data.List
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


class ShowStr a where
    showStr :: a -> String
    showListStr :: [a] -> String
    showListStr l = "[" <> intercalate ", " (map showStr l) <> "]"

instance ShowStr Char where
    showStr = show
    showListStr = id

instance ShowStr T.Text where
    showStr = T.unpack

instance ShowStr L.Text where
    showStr = L.unpack

instance {-# OVERLAPPABLE #-} Show a => ShowStr a where
    showStr = show


iS :: QuasiQuoter
iS = mqq { quoteExp = return . interpolateInto (VarE 'show) }
