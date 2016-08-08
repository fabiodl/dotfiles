{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.ShowText
-- Copyright   :  (c) Mario Pastorelli (2012)
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  pastorelli.mario@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- ShowText displays text for sometime on the screen similar to "XMonad.Util.Dzen"
-- which offers more features (currently)
-----------------------------------------------------------------------------

module  CenteredFlash
    ( -- * Usage
      -- $usage
      defaultSTConfig
    , handleTimerEvent
    , flashText
    , ShowTextConfig(..)
    ) where

import Control.Monad (when)
import Data.Map (Map,empty,insert,lookup)
import Data.Monoid (mempty, All)
import Prelude hiding (lookup)
import XMonad
import XMonad.StackSet (current,screen)
import qualified XMonad.StackSet as S

import XMonad.Util.Font (Align(AlignCenter)
                       , initXMF
                       , releaseXMF
                       , textExtentsXMF
                       , textWidthXMF)
import XMonad.Util.Timer (startTimer)
import XMonad.Util.XUtils (createNewWindow
                         , deleteWindow
                         , fi
                         , showWindow
                         , paintAndWrite)
import qualified XMonad.Util.ExtensibleState as ES

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.ShowText
--
-- Then add the event hook handler:
--
-- > xmonad { handleEventHook = myHandleEventHooks <+> handleTimerEvent }
--
-- You can then use flashText in your keybindings:
--
-- > ((modMask, xK_Right), flashText defaultSTConfig 1 "->" >> nextWS)
--

-- | ShowText contains the map with timers as keys and created windows as values
newtype ShowText = ShowText (Map Atom Window)
    deriving (Read,Show,Typeable)

instance ExtensionClass ShowText where
    initialValue = ShowText empty

-- | Utility to modify a ShowText
modShowText :: (Map Atom Window -> Map Atom Window) -> ShowText -> ShowText
modShowText f (ShowText m) = ShowText $ f m

data ShowTextConfig =
    STC { st_font :: String -- ^ Font name
        , st_bg   :: String -- ^ Background color
        , st_fg   :: String -- ^ Foreground color
    }

defaultSTConfig :: ShowTextConfig
defaultSTConfig =
    STC { st_font = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
        , st_bg   = "black"
        , st_fg   = "white"
    }

-- | Handles timer events that notify when a window should be removed
handleTimerEvent :: Event -> X All
handleTimerEvent (ClientMessageEvent _ _ _ dis _ mtyp d) = do
    (ShowText m) <- ES.get :: X ShowText
    a <- io $ internAtom dis "XMONAD_TIMER" False
    when (mtyp == a && length d >= 1)
         (whenJust (lookup (fromIntegral $ d !! 0) m) deleteWindow)
    mempty
handleTimerEvent _ = mempty

-- | Shows a window in the center of the screen with the given text
flashText :: ShowTextConfig
    -> Rational -- ^ number of seconds
    -> Position
    -> Position
    -> Position
    -> Position 
    -> String -- ^ text to display   
    -> X ()


flashText c i wh0 ht0 wh1 ht1 s = do
  f <- initXMF (st_font c)
  d <- asks display
  sc <- gets $ fi . screen . current . windowset
  width <- textWidthXMF d f s
  (as,ds) <- textExtentsXMF f s
  let height = as + ds
      y = if sc==0 then (fi ht0 - height + 2) `div` 2 else (fi ht1 - height + 2) `div` 2
      x = if sc==0 then (fi wh0 - width + 2) `div` 2 else (fi wh1 - width + 2) `div` 2 + (fi wh0)
  w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi height))
                      Nothing "" True
  showWindow w
 
  paintAndWrite w f (fi width) (fi height) 0 (st_bg c) ""
                (st_fg c) (st_bg c) [AlignCenter] [s]
  releaseXMF f
  io $ sync d False
  t <- startTimer i
  ES.modify $ modShowText (insert (fromIntegral t) w)
