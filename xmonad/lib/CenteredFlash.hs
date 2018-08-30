{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CenteredFlash
-- Copyright   :  (c) Mario Pastorelli (2012) , 
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
      def
    , handleTimerEvent
    , flashText'
    , ShowTextConfig(..)
    ) where

import Control.Monad (when)
import Data.Map (Map,empty,insert,lookup)
import Data.Monoid (mempty, All)
import Prelude hiding (lookup)
import XMonad
import XMonad.StackSet (current,screen)

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

import Graphics.X11.Xinerama



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

instance Default ShowTextConfig where
  def =
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



-- getScreenDim is from http://mntnoe.com/wp-content/uploads/2010/05/Panel.hs.html 
-- | Return the dimensions (x, y, width, height) of screen n.
getScreenDim :: Num a => Int -> IO (a, a, a, a)
getScreenDim n = do
    d <- openDisplay ""
    screens  <- getScreenInfo d
    closeDisplay d
    let rn = screens!!(min (abs n) (length screens - 1))
    case screens of
        []        -> return $ (0, 0, 1024, 768) -- fallback
        [r]       -> return $ (fromIntegral $ rect_x r , fromIntegral $ rect_y r , fromIntegral $ rect_width r , fromIntegral $ rect_height r )
        otherwise -> return $ (fromIntegral $ rect_x rn, fromIntegral $ rect_y rn, fromIntegral $ rect_width rn, fromIntegral $ rect_height rn)

-- | Shows a window in the center of the screen with the given text
flashText' :: ShowTextConfig
    -> Rational -- ^ number of seconds
    -> String -- ^ text to display   
    -> X ()

flashText' c i s = do
  f <- initXMF (st_font c)
  d <- asks display
  sc <- gets $ fi . screen . current . windowset
  (csx, csy, cswidth, csheight) <-liftIO $ getScreenDim sc
  width <- textWidthXMF d f s
  (as,ds) <- textExtentsXMF f s
  let height = as + ds
      x = csx + (cswidth -width +2) `div` 2
      y = csy + (csheight -fi height +2) `div` 2
  w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi height))
                      Nothing "" True
  showWindow w
 
  paintAndWrite w f (fi width) (fi height) 0 (st_bg c) ""
                (st_fg c) (st_bg c) [AlignCenter] [s]
  releaseXMF f
  io $ sync d False
  t <- startTimer i
  ES.modify $ modShowText (insert (fromIntegral t) w)
