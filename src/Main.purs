module Main where

import Data.String
import Data.String
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.Gen.Common (genEither)
import Control.Plus ((<|>))
import Control.Monad.Eff.Timer as T
import Control.Monad.Eff.Random as Rand
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (concat, concatMap, group, mapWithIndex, range, (..), (!!))
import Data.Map (empty)
import Data.Maybe (fromMaybe)
import Data.Record (get)
import Data.StrMap as M
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import FRP as F
import FRP.Behavior (step)
import FRP.Event.Time as Time
import FRP.Event as E
import FRP.Event.Mouse as ME
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util (getState, updateState, getRecordKey)
import UI.Util as U
import Data.Symbol
import Prelude
import Data.Foreign.Class (class Decode, class Encode, encode, decode )
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Traversable (sequence)

foreign import click :: MEvent
foreign import timerEvent :: MEvent
foreign import change :: MEvent
foreign import mousedown :: MEvent
foreign import mouseup :: MEvent

generateHoles state =
  mapWithIndex (\idx x -> holesRow state (innerLoop state x) idx) state.moles

innerLoop state row = map (\n -> holes state n) row

holes state id = imageView
                [ id_ id
                , height "100"
                , width "100"
                , imageUrl (getImageUrl $ (getRecordKey state id))
                , margin "20,20,20,20"
                , onClick (Some click)
                ]

holesRow state array id = linearLayout
                  [ id_ (show (id*1000))
                  , height "120"
                  , width "match_parent"
                  , gravity "center"
                  , orientation "horizontal"
                  ] array

widget state = relativeLayout
              [ id_ "1"
              , height "1000"
              , width "match_parent"
              , background "#262D38"
              , gravity "center"
              , orientation "vertical"
              ] [
                textView
                [ id_ "livesText"
                , color "#FFFFFF"
                , gravity "center"
                ,height "50"
                , width "match_parent"
                , margin "10,10,10,10"
                , textSize "20"
                , text ("Lives Remaining " <> state.lives)
                ],
                textView
                [ id_ "scoreText"
                , color "#FFFFFF"
                , gravity "center"
                ,height "50"
                , width "match_parent"
                , margin "10,30,10,10"
                , textSize "20"
                , text ("Score : " <> (show (state.score :: Int)))
                ],
                linearLayout
                [ id_ "5"
                , height "1000"
                , width "match_parent"
                , orientation "vertical"
                , margin "0,20,0,0"
                ]
                (generateHoles state),
                linearLayout
                [ id_ "GameOver"
                , height "500"
                , width "match_parent"
                , background "#262D38"
                , gravity "center"
                , visibility (getVisibility (state.gameOver))
                ] [textView
                [ id_ "GameOverText"
                , color "#FFFFFF"
                , gravity "center"
                ,height "50"
                , width "match_parent"
                , margin "10,10,10,10"
                , textSize "30"
                , text "Game Over. Reload the page to try again"
                ]]
              ]


data MoleState = START | UP | DOWN | HIT

derive instance genericProfileScreen :: Generic MoleState _
instance encodeProfileScreen :: Encode MoleState where
  encode = genericEncode defaultOptions
instance decodeProfileScreen :: Decode MoleState where
  decode = genericDecode defaultOptions


-- instance showMoleState :: Show MoleState
getImageUrl:: MoleState -> String
getImageUrl START = "start"
getImageUrl UP = "moleup"
getImageUrl DOWN = "moledown"
getImageUrl HIT = "molehit"


getVisibility false = "gone"
getVisibility true = "visible"


molesArray = ["101","201","202","301", "302", "303", "401", "402", "403", "404"]            

--main:: forall e. Eff ( dom :: DOM , console :: CONSOLE , frp :: FRP | e ) Unit
main = do
  --- Init State {} empty record--
  U.initializeState
  _ <- U.updateState "lives" 5
  _ <- U.updateState "gameOver" false
  _ <- U.updateState "score" 0

  let moles = [
              [401, 402, 403, 404]
              ,[301, 302, 303]
              ,[201, 202]
              ,[101]
              ]

  --- Update State ----
  _ <-sequence $ map (\x -> do 
                    U.updateState x START) molesArray

  ---- Render Widget ---
  state <- U.updateState "moles" moles 
  U.render (widget state) listen

getRandomDeactivatedMole molesArray = do
  val <- (Rand.randomInt 0 9)
  s <- U.getState
  idx <- pure $ fromMaybe "-1" $ (molesArray!!val) 
  case getRecordKey s idx of
    START -> pure idx
    HIT -> pure idx
    DOWN -> pure idx
    UP -> getRandomDeactivatedMole molesArray

evalTimer false = U.getState
evalTimer true = do
  state <- U.getState
  -- if state.lives == 0
  --   then
       
  --   else
  _ <-sequence $ map (\y -> do
                    let stateM = getRecordKey state y
                    case (getImageUrl stateM) of
                      "molehit" -> U.updateState y START
                      "moledown" ->  U.updateState y START
                      "moleup" -> do
                        st <- U.getState
                        _ <- U.updateState y DOWN
                        let newLive = st.lives - 1
                        _ <- U.updateState "lives" newLive
                        U.updateState "gameOver" (newLive <= 0)
                      _ -> U.getState) molesArray
  idx <- getRandomDeactivatedMole molesArray
  _ <- pure $ spy idx
  _ <- U.updateState idx UP
  U.getState

eval x = do
  s <- U.getState
  case getRecordKey s x.srcElement.id of
    UP -> do
      _ <- U.updateState "score" (s.score+1)
      U.updateState x.srcElement.id HIT
    _ -> pure s

listen = do
  let timeEvent = (Time.interval 100)
      timeBehavior = step 0 timeEvent
      moleBehavior = step 0 countStream
      countStream = E.fold (\t a -> a+1) timeEvent 0
  _ <- U.patch widget (evalTimer <$> (step false ((\c -> if ((mod c 20) == 0) then true else false) <$> countStream))) timeEvent
  sequence $ map (\mole -> do 
        signal <- U.signal mole {srcElement : {id : "0"}}
        U.patch widget (eval <$> signal.behavior) (signal.event)) molesArray

