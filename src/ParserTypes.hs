module ParserTypes where
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
  | CSI Char [String]
  | SGR [ColorCmd]
  deriving (Show, Eq)
