{-|
Module      : $Header$
Description : Interpolation and automatic conversion to lazy Text
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

Please refer to the documentation at https://marvin.readthedocs.io/en/latest/interpolation.html for examples and explanations on how to use this library.
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Interpolate.Text.Lazy
    ( isL, iqL
    -- * Conversion class
    , ShowL(..)
    ) where


import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


-- | A type class for converting things to 'L.Text'
--
-- Leaves string likes ('String', 'T.Text' and 'L.Text') unchanged, tries 'Show' (overlappable) on all others.
class ShowL a where
    showL :: a -> L.Text
    showListL :: [a] -> L.Text
    showListL l = "[" <> L.intercalate ", " (map showL l) <> "]"

instance ShowL a => ShowL [a] where
    showL = showListL

instance ShowL L.Text where
    showL = id

instance ShowL T.Text where
    showL = L.fromStrict

instance ShowL Char where
    showL = L.pack . show
    showListL = L.pack

instance {-# OVERLAPPABLE #-} Show a => ShowL a where
    showL = L.pack . show


-- | __i__nterpolate __s__plice to __L__azy Text
--
-- Template Haskell splice function, used like @$(isL "my str #{expr}")@
-- 
-- converts all expressions to 'L.Text' by calling 'showL' on the result.
isL :: String -> Q Exp
isL = return . interpolateInto (VarE 'showL)


-- | __i__nterpolate __q__uoter to __L__azy Text
--
-- QuasiQuoter, used like @[iqL|my str #{expr}|]@
--
-- converts all expressions to 'L.Text' by calling 'showL' on the result.
iqL :: QuasiQuoter
iqL = mqq { quoteExp = isL }
