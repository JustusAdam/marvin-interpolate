{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.Text.Lazy where


import Marvin.Interpolate
import Util
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Monoid


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


iLT :: QuasiQuoter
iLT = mqq { quoteExp = return . interpolateInto (VarE 'showLT) }
