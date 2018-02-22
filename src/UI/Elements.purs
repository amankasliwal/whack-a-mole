module UI.Elements
	( Node
	, Leaf
    , element
	, keyed
    , linearLayout
    , relativeLayout
    , horizontalScrollView
    , scrollView
    , imageView
    , editText
    , listView
    , progressBar
    , textView
    , viewPager
	) where


import Data.Maybe (Maybe(..))

import Data.Tuple (Tuple)

import Halogen.VDom (ElemName(..), ElemSpec(..), VDom(..))

import UI.Core (Attr(..), Prop)

type Node p i
   = Array Prop
  -> Array (VDom p i)
  -> VDom p i

type Leaf p i
   = Array Prop
  -> VDom p i

-- Keyed (ElemSpec a) (Array (Tuple String (VDom a w)))
element :: forall i. ElemName -> Array Prop -> Array (VDom Attr i) -> VDom Attr i
element elemName props = Elem (ElemSpec Nothing elemName (Attr props))

keyed :: forall i. ElemName -> Array Prop -> Array (Tuple String (VDom Attr i)) -> VDom Attr i
keyed elemName props children = Keyed (ElemSpec Nothing elemName (Attr props)) children

node :: forall i. String -> Node Attr i
node elem = element (ElemName elem)

leaf :: forall i. String -> Leaf Attr i
leaf elem props = element (ElemName elem) props []

linearLayout :: forall i. Node Attr i
linearLayout = node "linearLayout"

relativeLayout :: forall i. Node Attr i
relativeLayout = node "relativeLayout"

horizontalScrollView :: forall i. Node Attr i
horizontalScrollView = node "horizontalScrollView"

scrollView :: forall i. Node Attr i
scrollView = node "scrollView"

imageView :: forall i. Leaf Attr i
imageView = leaf "imageView"

editText :: forall i. Leaf Attr i
editText = leaf "editText"

listView :: forall i. Leaf Attr i
listView = leaf "listView"

progressBar :: forall i. Leaf Attr i
progressBar = leaf "progressBar"

textView :: forall i. Leaf Attr i
textView = leaf "textView"

viewPager :: forall i. Leaf Attr i
viewPager = leaf "viewPager"
