module UI.Elements.Keyed
	( KeyedNode
    , linearLayout
    , relativeLayout
    , horizontalScrollView
    , scrollView
	) where

import Data.Tuple (Tuple)

import Halogen.VDom (ElemName(ElemName), VDom)

import UI.Core (Attr, Prop)

import UI.Elements (keyed)

type KeyedNode p i
   = Array Prop
  -> Array (Tuple String (VDom p i))
  -> VDom p i


keyedNode :: forall i. String -> KeyedNode Attr i
keyedNode elem = keyed (ElemName elem)


linearLayout :: forall i. KeyedNode Attr i
linearLayout = keyedNode "linearLayout"

relativeLayout :: forall i. KeyedNode Attr i
relativeLayout = keyedNode "relativeLayout"

horizontalScrollView :: forall i. KeyedNode Attr i
horizontalScrollView = keyedNode "horizontalScrollView"

scrollView :: forall i. KeyedNode Attr i
scrollView = keyedNode "scrollView"


