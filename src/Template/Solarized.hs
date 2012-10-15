-- http://ethanschoonover.com/solarized

module Template.Solarized where

import Data.Text (Text)

-- base colors for fore- and backgrounds
b03, b02, b01, b00, b0, b1, b2, b03                             :: Text

-- accent colors
yellow, orange, red, magenta, violet, blue, cyan, green         :: Text

-- light mode
lightBg, lightBgHigh, lightTextLow, lightText, lightTextHigh    :: Text

-- dark mode
darkBg, darkBgHigh, darkTextLow, darkText, darkTextHigh         :: Text

b03             =   "#002b36"
b02             =   "#073642"
b01             =   "#586e75"
b00             =   "#657b83"
b0              =   "#839496"
b1              =   "#93a1a1"
b2              =   "#eee8d5"
b3              =   "#fdf6e3"

yellow          =   "#b58900"
orange          =   "#cb4b16"
red             =   "#dc322f"
magenta         =   "#d33682"
violet          =   "#6c71c4"
blue            =   "#268bd2"
cyan            =   "#2aa198"
green           =   "#859900"

lightBg         =                               b3
lightBgHigh     =                           b2
lightTextLow    =                       b1
lightText       =               b00
lightTextHigh   =           b01

darkBg          =   b03
darkBgHigh      =       b02
darkTextLow     =           b01
darkText        =                   b0
darkTextHigh    =                       b1
