{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Marvin.Interpolate
  ( interpolateInto
  , is
  , i
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


parser :: Parsec String () Parsed
parser = manyTill (parseInterpolation <|> parseString) eof

parseString :: Parsec String () (Either String String)
parseString = Right <$> parseTillEscape "%{" True

parseInterpolation :: Parsec String () (Either String String)
parseInterpolation = Left <$> between (try $ string "%{") (char '}') (parseTillEscape "}" False)

parseTillEscape :: String -> Bool -> Parsec String () String
parseTillEscape endSeq@(endChar:_) allowEOF = do
    chunk <- many $ noneOf [escapeChar, endChar]
    !rest <- eofEND
              <|> (char escapeChar >> parseEscaped)
              <|> (lookAhead (try $ string endSeq) >> return "")
              <|> (return <$> char endChar)
    return $ chunk <> rest
  where
    eofEND
        | allowEOF = eof >> return "" -- <|> (try (char escapeChar >> eof) >> char escapeChar >> return [escapeChar])
        | otherwise = fail "EOF not allowed in interpolation"

    parseEscaped = (eof >> return [escapeChar]) <|> do
        next <- anyChar
        let escaped
                | next == escapeChar = [escapeChar]
                | next == '%' = "%"
                | next == ']' = "]"
                | next == '}' = "}"
                | otherwise = escapeChar : [next]
        rest <- parseTillEscape endSeq allowEOF
        return $ escaped <> rest


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


interpolateInto :: Exp -> String -> Exp
interpolateInto converter str =
    foldl f (LitE (StringL "")) interleaved

  where
    parsed = either (error . show) id $ parse parser "inline" str
    interleaved = evalExprs parsed

    f expr bit = AppE (VarE 'mappend) expr `AppE` bitExpr
      where
        bitExpr = case bit of
                      Right str -> LitE (StringL str)
                      Left expr2 -> AppE converter expr2


is :: String -> Q Exp
is = return . interpolateInto (VarE 'id)


i :: QuasiQuoter
i = mqq { quoteExp = is }
