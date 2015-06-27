{-# LANGUAGE ScopedTypeVariables #-}
-- | Based on "man 4 console_codes"
module Parser where

import ParserTypes

import Data.Char
import Data.List.Split
import Control.Applicative
import Data.Maybe
import System.Glib.UTFString
import Control.Monad

stripCR :: String -> String
stripCR = filter (/= '\r')

isEsc :: Char -> Bool
isEsc = (== '\x1b')

parsePrim' :: String -> [PrimDisplayExpr]
parsePrim' [] = []
parsePrim' ss'@(s:ss)
  | isEsc s = next PrimControlSeq $ step $ break isLetter  ss
  | otherwise = next PrimText $ break isEsc ss'
  where next con (got, toGo) = (con got):(parsePrim toGo)
        step (rs, ls) = (rs ++ take 1 ls,  drop 1 ls)

parsePrim :: String -> [PrimDisplayExpr]
parsePrim = parsePrim' . stripCR

primBody :: PrimDisplayExpr -> String
primBody (PrimText s) = s
primBody (PrimControlSeq s) = s

processPrim :: PrimDisplayExpr -> [DisplayExpr]
processPrim pExp = getRes res
  where res = processPrim' pExp
        fallback = [Text $ primBody pExp]
        getRes [] = fallback
        getRes _ = res

-- | Recognize a character that ends an OSC sequence
-- FIXME: We don't support "ESC-\". See
-- http://www.databeast.com/datacomet/Documents/1.1._VT100_Command_Set.txt
isStringTerminator :: Char -> Bool
isStringTerminator '\157' = True
isStringTerminator '\a' = True
isStringTerminator _ = False

stripTrailingSemiColon :: String -> String
stripTrailingSemiColon s = reverse backwards
  where (';':backwards) = reverse s

-- FIXME: strip out the Maybe here
processPrim' :: PrimDisplayExpr -> [DisplayExpr]
processPrim' (PrimText t) = [Text t]
processPrim' (PrimControlSeq (']':s)) = (map OSC $ mkOSCmd osCmd) ++ rest'
  where (osCmdSimple, rest) = break isStringTerminator s
        osCmd = stripTrailingSemiColon osCmdSimple
        rest' = join $ map processPrim $ parsePrim rest
processPrim' (PrimControlSeq ('[':s)) = fromMaybe [] ((:[]) <$> mkControlSeq name body)
  where name = last s
        body = splitOn ";" $ init s
-- We don't support it, pass it on
processPrim' _ = []


mkOSCmd :: String -> [OSCmd]
mkOSCmd = mkOSCmd' . splitOn ";"


mkOSCmd' :: [String] -> [OSCmd]
mkOSCmd' ["0", title] = [IconName title, WindowTitle title]
mkOSCmd' ["1", title] = [WindowTitle title]
mkOSCmd' ["2", name] = [IconName name]
mkOSCmd' _ = []

mkControlSeq :: Char -> [String] -> Maybe DisplayExpr
mkControlSeq 'm' = (SGR <$>) . mapM mkColorCmd
mkControlSeq c = Just . CSI c

mkColorCmd :: String -> Maybe ColorCmd
mkColorCmd "0" = Just Reset
mkColorCmd "01" = Just Bold -- FIXME: What is this?
-- mkColorCmd [isBg, color] = Just $ Set (mkColor color, mkPos isBg)
-- mkColorCmd [isBg, color] =  mkPos isBg >>= (,) mkColor color >>= return . Set
mkColorCmd [isBg, color] =  Set <$> ((,) <$> mkColor color  <*> mkPos isBg)
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
parse = join . map processPrim . parsePrim . stripCR

colorForGtk :: Color -> String
colorForGtk = (map toLower) . show
