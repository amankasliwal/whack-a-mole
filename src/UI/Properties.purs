module UI.Properties where

import Prelude

import Data.Tuple (Tuple(..))

import UI.Core (AttrValue(..), Prop)


prop :: String -> String -> Prop
prop key value = Tuple key (AttrValue value)

id_ :: String -> Prop
id_ = prop "id"

height :: String -> Prop
height = prop "height"

visibility :: String -> Prop
visibility = prop "visibility"

width :: String -> Prop
width = prop "width"

background :: String -> Prop
background = prop "background"

imageUrl :: String -> Prop
imageUrl = prop "imageUrl"

margin :: String -> Prop
margin = prop "margin"

padding :: String -> Prop
padding = prop "padding"

color :: String -> Prop
color = prop "color"

text :: String -> Prop
text = prop "text"

bg :: String -> Prop
bg = prop "bg"

gravity :: String -> Prop
gravity = prop "gravity"

orientation :: String -> Prop
orientation = prop "orientation"

fontStyle :: String -> Prop
fontStyle = prop "fontStyle"

textSize :: String -> Prop
textSize = prop "textSize"

textWeight :: String -> Prop
textWeight = prop "textWeight"

cornerRadius :: String -> Prop
cornerRadius = prop "cornerRadius"

hint :: String -> Prop
hint = prop "hint"

stroke :: String -> Prop
stroke = prop "stroke"
