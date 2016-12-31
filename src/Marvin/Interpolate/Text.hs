{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.Text where


import Marvin.Interpolate
import Util
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import Data.Monoid


class ShowT a where
    showT :: a -> Text
    showListT :: [a] -> Text
    showListT l = "[" <> intercalate ", " (map showT l) <> "]"

instance ShowT Text where
    showT = id

instance ShowT L.Text where
    showT = L.toStrict

instance ShowT Char where
    showT = pack . show
    showListT = pack

instance {-# OVERLAPPABLE #-} Show a => ShowT a where
    showT = pack . show


iT :: QuasiQuoter
iT = mqq { quoteExp = return . interpolateInto (VarE 'showT) }
