{-|
Module      : $Header$
Description : Interpolation and automatic conversion to Text
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
module Marvin.Interpolate.Text 
    ( isT, iqT
    -- * Conversion class
    , ShowT(..)
    ) where


import           Data.Monoid
import           Data.Text                 hiding (map)
import qualified Data.Text.Lazy            as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Marvin.Interpolate
import           Util


-- | A type class for converting things to 'Text'
--
-- Leaves string likes ('String', 'Text' and 'L.Text') unchanged, tries 'Show' (overlappable) on all others.
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


-- | __i__nterpolate __s__plice to __T__ext
--
-- Template Haskell splice function, used like @$(isT "my str #{expr}")@
-- 
-- converts all expressions to 'Text' by calling 'showT' on the result.
isT :: String -> Q Exp
isT = return . interpolateInto (VarE 'showT)


-- | __i__nterpolate __q__uoter to __T__ext
--
-- QuasiQuoter, used like @[iqT|my str #{expr}|]@
--
-- converts all expressions to 'Text' by calling 'showT' on the result.
iqT :: QuasiQuoter
iqT = mqq { quoteExp = isT }
