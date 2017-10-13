{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.Tabbed

import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import System.IO
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Prompt.Window

import XMonad.Util.Scratchpad
import Data.Maybe
import XMonad.Util.Loggers (logCurrent)
import Data.Monoid
import XMonad.Layout.IndependentScreens

import XMonad.Prompt.Shell
import qualified Data.List as DL

import XMonad.Hooks.FadeInactive
import Data.Function (on)

import qualified Data.Text.Lazy as TL

import CenteredFlash
import XMonad.Actions.Navigation2D

import Data.Time.LocalTime
import Text.Printf
import XMonad.Util.XUtils



myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $ [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
   
    , ((0,13 :: Button) , ( \w -> spawn "google-chrome" ))
    , ((0,10 :: Button) , ( \w -> spawn "gnome-terminal" )) 
    ]



myManageHook = composeAll
    [ className =? "Gimp"        --> doFloat
    , className =? "Vncviewer"   --> doFloat
    , className =? "stalonetray" --> doIgnore
    , className =? "com-mathworks-util-PostVMInit" --> doFloat
    , className =? "Cairo-dock" --> doFloat
    , className =? "platform-Emulicious" -->doFloat
    ]  <+> scratchpadManageHook (W.RationalRect 0 0 1 0.3)


myWorkspaces = map show [1..9]

myBgColor = "black"
myFgColor = "skyblue"

--for Prompt
myXPConfig = def{
  fgColor=myFgColor
  , bgColor=myBgColor
  , fgHLight=myBgColor
  , bgHLight=myFgColor
  , borderColor=myFgColor
  , searchPredicate = \a b -> DL.isInfixOf (toUpper a) (toUpper b) 
  , alwaysHighlight = True
  , maxComplRows=Just 3 --not available in version 0.11
  }



myModKey= mod4Mask                  


scratchPad = scratchpadSpawnActionTerminal "urxvt"
dmenu c= "dmenu_run -nb \""++(bgColor c)++"\" -nf \""++(fgColor c)++"\" -sb \""++(fgColor c)++"\" -sf \""++(bgColor c)++"\""
modm=mod4Mask
myKeys=
 [ ((mod1Mask .|. shiftMask , xK_BackSpace), spawn "gnome-screensaver-command -l")
 , ((myModKey .|. shiftMask,   xK_q), spawn "xkill")
 , ((myModKey .|. shiftMask .|. controlMask,  xK_q), io exitSuccess)
 , ((myModKey , xK_g     ), printWs >> colourXP windowPromptGoto  >> printWs)
 , ((myModKey , xK_b     ), printWs >> colourXP windowPromptBring)
 , ((myModKey , xK_p     ), colourXP shellPrompt)
 , ((myModKey .|. shiftMask , xK_p), colourXP (spawn . dmenu) ) 
 , ((myModKey , xK_n),  moveTo Next HiddenNonEmptyWS >> printWs)
 , ((myModKey .|. shiftMask, xK_n), moveTo Prev HiddenNonEmptyWS >> printWs)
 , ((myModKey .|. controlMask, xK_n),  moveTo Next (WSIs newWorkspace) >> printWs)
 -- , ((myModKey .|. shiftMask, xK_Up), swapNextScreen >> printWs)
 , ((myModKey , xK_i), spawn "google-chrome")
 , ((myModKey, xK_F12), scratchPad) -- quake terminal
 , ((myModKey , xK_d), spawn "gjiten")
 , ((myModKey, xK_s), sendMessage (Toggle REFLECTX) >> printWs)
 , ((myModKey .|. shiftMask, xK_s), screenSwap L True >>printWs)
 ]
  ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((myModKey .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f) >>printWs)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift , shiftMask)]]
  ++
  [
    ((myModKey .|. m, k),   (windows $ f i) >> printWs) 
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    , (f, m) <- [(lazyView, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
  ]
  ++
  [
  (( myModKey .|. m,k), a dir loop) |
    (a,m,loop) <- [ (windowGo,0,False), (windowSwap,shiftMask,False), (windowToScreenMaster,controlMask,False), (screenGo,mod1Mask,True)] ,
    (k,dir) <-[ (xK_Right, R), (xK_Left, L), (xK_Up, U), (xK_Down, D) ] 
  ]
  where windowToScreenMaster dir loop = windowToScreen dir loop >> screenGo dir loop >> windows W.swapMaster

fadeHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.9

  
main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withNavigation2DConfig def $ ewmh gnomeConfig
         { logHook = dynamicLogWithPP (prettyPrinter dbus) <+> (clockColor >>= setBorderColor)-- <+> fadeHook
         , mouseBindings = myMouseBindings
         , layoutHook =  smartBorders (   mkToggle (single REFLECTX) $ layoutHook gnomeConfig) ||| tabbed shrinkText  defaultTheme
         , normalBorderColor   =  myBgColor
         , focusedBorderColor =  myFgColor
         , modMask = myModKey 
         , startupHook = setWMName "LG3D"
         , handleEventHook = handleEventHook gnomeConfig <+> fullscreenEventHook <+> handleTimerEvent
         , manageHook = myManageHook <+> manageHook gnomeConfig
         , workspaces = myWorkspaces
         } `additionalKeys` myKeys 


prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoColor "white" . pangoFontWrap "" "" .pangoSanitize
    , ppCurrent  = pangoColor "gold" . pangoFontWrap"{" "}" . pangoSanitize
    , ppVisible  = pangoColor "lightsalmon" . pangoFontWrap "[" "]" . pangoSanitize
    , ppHidden   = pangoColor "white" . pangoFontWrap "(" ")" . pangoSanitize . onlyKnown
    , ppUrgent   = pangoColor "red"
    , ppLayout   = pangoColor "seagreen" . pangoFontWrap "|" "|"
    , ppSep      = pangoFontWrap "" "" " "
    }
 where
    onlyKnown ws = if ws `elem` myWorkspaces then ws else ""


getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoFontWrap :: String -> String -> (String->String)
pangoFontWrap open close = wrap left right
  where
    left="<span font=\"Sans Bold 8\">"++open
    right=close++"</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


toUpper :: String -> String
toUpper = TL.unpack . TL.toUpper .  TL.pack


cmpScreen' :: Rectangle -> Rectangle -> Ordering
cmpScreen' (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (y1,x1) (y2,x2)

getOrderedScreens :: X [ScreenId]
getOrderedScreens =do w <- gets windowset
                      return (map W.screen $ DL.sortBy (cmpScreen' `on` (screenRect . W.screenDetail)) $ W.current w : W.visible w)


newWorkspace:: X (WindowSpace -> Bool)
newWorkspace = do hs <- gets (map W.tag . W.hidden . windowset)
                  let empty = isNothing . W.stack
                  let hidden = (\w -> W.tag w `elem` hs)
                  let knownWs = (\w -> W.tag w `elem` myWorkspaces) 
                  return (\w -> hidden w && empty w && knownWs w)


--adapted from http://xmonad.haskell.narkive.com/EToEJM1K/normal-rather-than-greedy-view-disable-screen-focus-switching
isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws


setBorderColor :: String -> X ()
setBorderColor col = do d <- asks display
                        px <- stringToPixel d col           
                        ws <- gets (W.peek . windowset) --focused Window
                        case ws of
                          Nothing -> return ()
                          Just win -> setWindowBorderWithFallback d win col px


getWorkspacesString :: X String
getWorkspacesString = do w <- gets windowset                         
                         screens <- getOrderedScreens
                         return $ foldr (++) "" (DL.intersperse " " [ highlight (W.currentTag w)  (fromMaybe "" $ W.lookupWorkspace sc w)  | sc<-screens]) where highlight curr ws = if curr==ws then "["++ws++"]" else ws
                         


colourXP :: (XPConfig -> X() )  -> X()
colourXP f = do c <-clockColor
                f myXPConfig{fgColor=c
                            ,bgHLight=c
                            ,borderColor=c
                            }

printWs :: X ()
printWs = do text <- getWorkspacesString
             c <-clockColor
             flashText' defaultSTConfig{st_bg=myBgColor, st_fg=c} 1 text

clockColor :: X String
clockColor = do now<-io getTime
                return (timeToColor now)


timeToColor :: TimeOfDay-> String
timeToColor time = let maxTime = 3600.0*23.0+60.0*59+61.0  :: Float
                       hue = 2*360.0/maxTime *(  3600.0*(fromIntegral $ todHour time ) --2 cycles a day
                                              +  60.0* (fromIntegral $ todMin time)
                                              + realToFrac(todSec time))   :: Float
                       color = hsv hue 0.43 0.92
                       [r,g,b]= map (round . (*255)) color ::[Integer]
                   in printf "#%02X%02X%02X" r g b 

getTime :: IO TimeOfDay                      
getTime = fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime



hsv :: (RealFrac a, Ord a) => a -> a -> a -> [a]
hsv h s v = case hi of
    0 -> [v,t,p]
    1 -> [q,v,p]
    2 -> [p,v,t]
    3 -> [p,q,v]
    4 -> [t,p,v]
    5 -> [v,p,q]
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

mod1 x | pf < 0 = pf+1
       | otherwise = pf
 where
  (_,pf) = properFraction x
