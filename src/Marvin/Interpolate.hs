{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Marvin.Interpolate where


import Text.Parsec
import Control.Monad
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.Meta.Parse.Careful
import Control.Monad.State as S
import Data.List (intercalate)
import Data.Either
import Language.Haskell.TH.Quote
import Util


type Parsed = [Either String String]


parser :: Parsec String () Parsed
parser = manyTill (parseInterpolation <|> parseString) eof

parseString :: Parsec String () (Either String String)
parseString = Right <$> parseTillEscape '%' True

parseInterpolation :: Parsec String () (Either String String)
parseInterpolation = Left <$> between (string "%{") (char '}') (parseTillEscape '}' False)

parseTillEscape :: Char -> Bool -> Parsec String () String
parseTillEscape endChar allowEOF = do 
    chunk <- many $ noneOf ['\\', endChar]
    rest <- eofEND <|> (lookAhead (try anyChar) >>= restEND)
    return $ chunk <> rest
  where
    eofEND
        | allowEOF = eof >> return ""
        | otherwise = fail "EOF not allowed in interpolation"
    
    restEND c
        | c == '\\' = parseEscaped
        | c == endChar = return ""

    parseEscaped = do
        char '\\'
        next <- anyChar
        let escaped 
                | next == '\\' = "\\" ++ [next]
                | next == endChar = [endChar]
        rest <- parseTillEscape endChar allowEOF
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


i :: QuasiQuoter
i = mqq { quoteExp = return . interpolateInto (VarE 'id) }
