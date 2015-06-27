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
  | Bold
  | Set (Color, ColorPos)
  deriving (Show, Eq)

-- Here, we will handle "ESC ] 0" (set both icon name + window title)
-- by replacing it with two separate commands.
data OSCmd
  = IconName String
  | WindowTitle String
  deriving (Show, Eq)
  -- Not supported:
  -- ColorOverride Integer String
  -- DynamicTextColor String
  -- Font String

data PrimDisplayExpr
  = PrimText String
  | PrimControlSeq String
  deriving (Show, Eq)

data DisplayExpr
  = Text String
  | CSI Char [String] -- Control Sequence (Initiator)
  | SGR [ColorCmd] -- Select Graphic Rendition i.e. colors, fonts
  | OSC OSCmd
  deriving (Show, Eq)
