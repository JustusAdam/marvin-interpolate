{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.Text where


import           Data.Monoid
import           Data.Text                 hiding (map)
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


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


isT :: String -> Q Exp
isT = return . interpolateInto (VarE 'showT)


iT :: QuasiQuoter
iT = mqq { quoteExp = isT }
