{-|
Module      : $Header$
Description : Interpolation and automatic conversion to String
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
module Marvin.Interpolate.String
    ( isS, iqS
    -- * Conversion class
    , ShowStr(showStr)
    ) where


import           Data.List
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


-- | A type class for converting things to 'String'
--
-- Leaves string likes ('String', 'T.Text' and 'L.Text') unchanged, and tries 'Show' (overlappable) on all others.
class ShowStr a where
    showStr :: a -> String
    showListStr :: [a] -> String
    showListStr l = "[" <> intercalate ", " (map showStr l) <> "]"

    {-# MINIMAL showStr #-}

instance ShowStr Char where
    showStr = show
    showListStr = id

instance ShowStr a => ShowStr [a] where
    showStr = showListStr

instance ShowStr T.Text where
    showStr = T.unpack

instance ShowStr L.Text where
    showStr = L.unpack

instance {-# OVERLAPPABLE #-} Show a => ShowStr a where
    showStr = show


-- | __i__nterpolate __s__plice to __S__tring
--
-- Template Haskell splice function, used like @$(isS "my str #{expr}")@
-- 
-- converts all expressions to 'String' by calling 'showStr' on the result.
isS :: String -> Q Exp
isS = return . interpolateInto (VarE 'showStr)


-- | __i__nterpolate __q__uoter to __S__tring
--
-- QuasiQuoter, used like @[iqS|my str #{expr}|]@
--
-- converts all expressions to 'String' by calling 'showStr' on the result.
iqS :: QuasiQuoter
iqS = mqq { quoteExp = isS }
