module UI.Events where

import Prelude

import Data.Tuple (Tuple(..))

import UI.Core (AttrValue(..), Prop)

domName :: AttrValue -> Prop
domName st = Tuple "domName" st

onClick :: AttrValue -> Prop
onClick some = Tuple "onClick" some

onChange :: AttrValue -> Prop
onChange some = Tuple "onChange" some


onMouseDown :: AttrValue -> Prop
onMouseDown some = Tuple "onMouseDown" some

onMouseUp :: AttrValue -> Prop
onMouseUp some = Tuple "onMouseUp" some
