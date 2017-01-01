{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.Text.Lazy where


import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


class ShowLT a where
    showLT :: a -> L.Text
    showListLT :: [a] -> L.Text
    showListLT l = "[" <> L.intercalate ", " (map showLT l) <> "]"

instance ShowLT L.Text where
    showLT = id

instance ShowLT T.Text where
    showLT = L.fromStrict

instance ShowLT Char where
    showLT = L.pack . show
    showListLT = L.pack

instance {-# OVERLAPPABLE #-} Show a => ShowLT a where
    showLT = L.pack . show


isLT :: String -> Q Exp
isLT = return . interpolateInto (VarE 'showLT)


iLT :: QuasiQuoter
iLT = mqq { quoteExp = isLT }
