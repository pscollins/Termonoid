{-# LANGUAGE ScopedTypeVariables #-}
-- | Based on "man 4 console_codes"
module Parser where

import ParserTypes

import Data.Char
import Data.List.Split
import Control.Applicative
import Data.Maybe

parsePrim :: String -> [PrimDisplayExpr]
parsePrim [] = []
parsePrim ss'@(s:ss)
  | isControl s = next PrimControlSeq $ step $ break isLetter ss
  | otherwise = next PrimText $ break isControl ss'
  where next con (got, toGo) = (con got):(parsePrim toGo)
        step (rs, ls) = (rs ++ take 1 ls,  drop 1 ls)

primBody :: PrimDisplayExpr -> String
primBody (PrimText s) = s
primBody (PrimControlSeq s) = s

processPrim :: PrimDisplayExpr -> DisplayExpr
processPrim pExp = fromMaybe (Text $ primBody pExp) $ processPrim' pExp


processPrim' :: PrimDisplayExpr -> Maybe DisplayExpr
processPrim' (PrimText t) = Just $ Text t
processPrim' (PrimControlSeq ('[':s)) = mkControlSeq name body
  where name = last s
        body = splitOn ";" $ init s
-- We don't support it, pass it on
processPrim' _ = Nothing


mkControlSeq :: Char -> [String] -> Maybe DisplayExpr
mkControlSeq 'm' = (SGR <$>) . mapM mkColorCmd
mkControlSeq c = Just . CSI c

mkColorCmd :: String -> Maybe ColorCmd
mkColorCmd "0" = Just Reset
-- mkColorCmd [isBg, color] = Just $ Set (mkColor color, mkPos isBg)
-- mkColorCmd [isBg, color] =  mkPos isBg >>= (,) mkColor color >>= return . Set
mkColorCmd [isBg, color] =  Set <$> ((,) <$> mkColor color  <*> mkPos isBg )
mkColorCmd _ = Nothing


mkPos :: Char -> Maybe ColorPos
mkPos '3' = Just Foreground
mkPos '4' = Just Background
mkPos _ = Nothing

mkColor :: Char -> Maybe Color
mkColor '0' = Just Black
mkColor '1' = Just Red
mkColor '2' = Just Green
mkColor '3' = Just Brown
mkColor '4' = Just Blue
mkColor '5' = Just Magenta
mkColor '6' = Just Cyan
mkColor '7' = Just White
mkColor _ = Nothing



parse :: String -> [DisplayExpr]
parse = map processPrim . parsePrim
