{-# LANGUAGE ScopedTypeVariables #-}
-- | Based on "man 4 console_codes"
module Parser where

import Data.Char


data Color
  = Black
  | Red
  | Green
  | Brown
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Eq)

data ColorPos
  = Foreground
  | Background
  deriving (Show, Eq)

data ColorCmd
  = Reset -- other stuff to, TODO
  | Set (Color, ColorPos)
  deriving (Show, Eq)

data PrimDisplayExpr
  = PrimText String
  | PrimControlSeq String
  deriving (Show, Eq)

data DisplayExpr
  = Text String
  | CSI [Int] Char
  | SGR [ColorCmd]
  deriving (Show, Eq)

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


parse :: String -> [DisplayExpr]
parse = undefined
