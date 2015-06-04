{-# LANGUAGE ScopedTypeVariables #-}
-- | Based on "man 4 console_codes"
module Parser where

import ParserTypes

import Data.Char
import Data.List.Split
import Debug.Trace

esc :: Char
esc = '\x1b'

csi :: String
csi = esc:"["


parsePrim :: String -> [PrimDisplayExpr]
parsePrim [] = []
parsePrim ss'@(s:ss)
  | isControl s = next PrimControlSeq $ step $ break isLetter ss
  | otherwise = next PrimText $ break isControl ss'
  where next con (got, toGo) = (con got):(parsePrim toGo)
        step (rs, ls) = (rs ++ take 1 ls,  drop 1 ls)

-- | This is valid for all well-formed control sequences. We put our
-- faith in Bash.
processPrim :: PrimDisplayExpr -> DisplayExpr
processPrim (PrimText t) = Text t
processPrim (PrimControlSeq ('[':s)) = mkControlSeq name body
  where name = last s
        body = splitOn ";" $ init s
processPrim _= undefined -- We don't support it


mkControlSeq :: Char -> [String] -> DisplayExpr
mkControlSeq 'm' = SGR . map mkColorCmd
mkControlSeq c = CSI c

mkColorCmd :: String -> ColorCmd
mkColorCmd "0" = Reset
mkColorCmd [isBg, color] = Set (mkColor color, mkPos isBg)


mkPos :: Char -> ColorPos
mkPos '3' = Foreground
mkPos '4' = Background

mkColor :: Char -> Color
mkColor '0' = Black
mkColor '1' = Red



parse :: String -> [DisplayExpr]
parse = map processPrim . parsePrim
