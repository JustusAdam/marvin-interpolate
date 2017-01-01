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
    ( isLT, iqLT
    -- * Conversion class
    , ShowLT(..)
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


-- | __i__nterpolate __s__plice to __L__azy __T__ext
--
-- Template Haskell splice function, used like @$(isLT "my str %{expr}")@
-- 
-- converts all expressions to 'L.Text' by calling 'showLT' on the result.
isLT :: String -> Q Exp
isLT = return . interpolateInto (VarE 'showLT)


-- | __i__nterpolate __q__uoter to __L__azy __T__ext
--
-- QuasiQuoter, used like @[iLT|my str %{expr}|]@
--
-- converts all expressions to 'L.Text' by calling 'showLT' on the result.
iqLT :: QuasiQuoter
iqLT = mqq { quoteExp = isLT }
