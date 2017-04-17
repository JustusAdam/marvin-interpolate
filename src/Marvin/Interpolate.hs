{-|
Module      : $Header$
Description : Interpolation core module
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

Please refer to the documentation at https://marvin.readthedocs.io/en/latest/interpolation.html for examples and explanations on how to use this library.
-}
{-# LANGUAGE TemplateHaskell #-}
module Marvin.Interpolate
  ( is
  , iq
  -- * Internals/extension points
  , interpolateInto
  ) where


import           Control.Monad
import           Control.Monad.State                 as S
import           Data.Either
import           Data.List                           (intercalate)
import           Data.Monoid
import           Language.Haskell.Meta.Parse.Careful
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec
import           Util


type Parsed = [Either String String]


escapeChar :: Char
escapeChar = '~'

type ParseM = Parsec String Int


parser :: ParseM Parsed
parser = manyTill (parseInterpolation <|> parseString) eof

parseString :: ParseM (Either String String)
parseString = do
    chunk <- many $ satisfy (/= '#')
    fmap (Right . (chunk ++)) $ (eof >> return "") <|> (lookAhead (try (char '#' >> anyChar)) >>= endOrEscape) <|> fmap return anyChar
  where
    endOrEscape :: Char -> ParseM String
    endOrEscape '{' = return ""
    endOrEscape '#' = count 2 anyChar >> return "#"
    endOrEscape ']' = count 2 anyChar >> return "]"
    endOrEscape _ = fail ""


parseInterpolation :: ParseM (Either String String)
parseInterpolation = try (string "#{") >> (Left <$> parseExpr)
  where
    parseExpr = do
        chunk <- many $ noneOf ['}', '"', '\'', '{']
        fmap (chunk ++) $ (eof >> error "eof in interpolation") <|> (anyChar >>= continue)

    continue :: Char -> ParseM String
    continue '{' = modifyState succ >> fmap ('{':) parseExpr
    continue '}' = do
        s <- getState
        if s == 0
          then return ""
          else ('}':) <$> (modifyState succ >> parseExpr)
    continue '\"' = ('"':) <$> parseStr
    continue '\'' = parseChar <|> (('\'':) <$> parseExpr)
      
    parseChar = do
        char '\''
        inner <- ((:) <$> char '\\' <*> fmap return anyChar) <|> fmap return anyChar
        char '\''
        return $ '\'':inner ++ "'"

    parseStr = do
        chunk <- many $ noneOf ['"', '\\']
        fmap (chunk ++) $ (eof >>= fail "eof in string literal") 
                          <|> (anyChar >>= continueStr)
      where 
        continueStr '"' = ('"':) <$> parseExpr
        continueStr '\\' = do
            escaped <- anyChar
            (\a -> '\\':escaped:a) <$> parseStr


evalExprs :: Parsed -> [Either Exp String]
evalExprs l = evalState (mapM stitch l) decls
  where
    strDecls = lefts l
    decls = case partitionEithers $ map parseExp strDecls of
                ([], d) -> d
                (errs, _) -> error $ intercalate "\n" errs

    stitch :: Either a b -> S.State [c] (Either c b)
    stitch (Right str) = return $ Right str
    stitch (Left _) = do
        (name:rest) <- get
        put rest
        return $ Left name


-- | Common core of all interpolators.
--
-- @interpolateInto exp str@ parses @str@ as the interpolated string and returns an 'Exp' which looks like 
-- 
-- @
--    "str" \`mappend\` exp1 \`mappend\` "str" \`mappend\` exp2 \`mappend\` "str"
-- @ 
-- 
-- where @exp1@ and @exp2@ are the interpolated expressions with @exp@ prepended.
-- The intended use of @exp@ is to unifomly convert the interpolated expressions into a desired string type.
-- Typically @exp@ will be something like @('VarE' \'convert)@ were @convert@ is some member function of a conversion type class.
interpolateInto :: Exp -> String -> Exp
interpolateInto converter str =
    foldl f (LitE (StringL "")) interleaved

  where
    parsed = either (error . show) id $ runParser parser 0 "inline" str
    interleaved = evalExprs parsed

    f expr bit = AppE (VarE 'mappend) expr `AppE` bitExpr
      where
        bitExpr = case bit of
                      Right str -> LitE (StringL str)
                      Left expr2 -> AppE converter expr2

-- | __i__nterpolate __s__plice 
--
-- Template Haskell splice function, used like @$(is "my str #{expr}")@
-- 
-- Performs no conversion on interpolated expressions like @expr@.
is :: String -> Q Exp
is = return . interpolateInto (VarE 'id)


-- | __i__nterpolate __q__uoter
--
-- QuasiQuoter, used like @[iq|my str #{expr}|]@
--
-- Performs no conversion on interpolated expressions like @expr@.
iq :: QuasiQuoter
iq = mqq { quoteExp = is }
