{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.NoBorders
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


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $ [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
   
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
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

mySTConfig=defaultSTConfig{
  st_fg=myFgColor
  , st_bg=myBgColor
  }

myModKey= mod4Mask                  

scratchPad = scratchpadSpawnActionTerminal "urxvt"
dmenu = "dmenu_run -nb "++myBgColor++" -nf "++myFgColor++" -sb "++myFgColor++" -sf "++myBgColor 

myKeys=
 [ ((mod1Mask .|. shiftMask , xK_BackSpace), spawn "gnome-screensaver-command -l")
 , ((myModKey .|. shiftMask,   xK_q), spawn "xkill")
 , ((myModKey .|. shiftMask .|. controlMask,  xK_q), io exitSuccess)
 , ((myModKey , xK_g     ), windowPromptGoto  myXPConfig  >> printWs)
 , ((myModKey , xK_b     ), windowPromptBring myXPConfig)
 , ((myModKey , xK_p     ), shellPrompt myXPConfig)
 , ((myModKey .|. shiftMask , xK_p), spawn dmenu) 
 , ((myModKey , xK_Left), pushWindow (-1) >> windows W.swapMaster)
 , ((myModKey , xK_Right), pushWindow 1 >> windows W.swapMaster)
 , ((myModKey , xK_Up), sendMessage (Toggle REFLECTX) >> printWs)
 , ((myModKey , xK_n),  moveTo Next HiddenNonEmptyWS >> printWs)
 , ((myModKey .|. shiftMask, xK_n), moveTo Prev HiddenNonEmptyWS >> printWs)
 , ((myModKey .|. controlMask, xK_n),  moveTo Next (WSIs newWorkspace) >> printWs)
 , ((myModKey .|. shiftMask, xK_Up), swapNextScreen >> printWs)
 , ((myModKey , xK_i), spawn "google-chrome")
 , ((myModKey, xK_F12), scratchPad) -- quake terminal
 , ((myModKey , xK_d), spawn "gjiten")   
        ]
 ++
        [
  ((m .|. myModKey, k),   (windows $ f i) >> printWs) 
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(lazyView, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
        ]

fadeHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.9

  
main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ ewmh gnomeConfig
         { logHook = dynamicLogWithPP (prettyPrinter dbus) -- <+> fadeHook
         , mouseBindings = myMouseBindings
         , layoutHook = smartBorders (   mkToggle (single REFLECTX) $ layoutHook gnomeConfig) ||| tabbed shrinkText  defaultTheme
         , normalBorderColor   =  myBgColor
         , focusedBorderColor =  myFgColor
         , modMask = myModKey -- set the mod key to the windows key
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
    , ppSep      = pangoFontWrap "" ""
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


-- variation of XMonad.Actions.PhysicalScreens, the looping behavior of getNeighbour (left of leftmost is rightmost) is removed

cmpScreen' :: Rectangle -> Rectangle -> Ordering
cmpScreen' (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (y1,x1) (y2,x2)

getOrderedScreens :: X [ScreenId]
getOrderedScreens =do w <- gets windowset
                      return (map W.screen $ DL.sortBy (cmpScreen' `on` (screenRect . W.screenDetail)) $ W.current w : W.visible w)

getNeighbour' :: Int -> X ScreenId
getNeighbour' direction = do w <- gets windowset
                             ss <- getOrderedScreens
                             let curPos = maybe 0 id $ DL.findIndex (== W.screen (W.current w)) ss
                                 posL= max 0 (curPos + direction)                          
                                 pos = min posL (length ss)  
                             return $ ss !! pos
pushWindow :: Int -> X ()
pushWindow direction = do s <- getNeighbour' direction
                          w <- screenWorkspace s
                          whenJust w $ windows . W.shift  
                          whenJust w $ windows . W.view

--for looping to empty, hidden and "standard" (no scratchpad etc.) workspaces

newWorkspace:: X (WindowSpace -> Bool)
newWorkspace = do hs <- gets (map W.tag . W.hidden . windowset)
                  let empty = isNothing . W.stack
                  let hidden = (\w -> W.tag w `elem` hs)
                  let knownWs = (\w -> W.tag w `elem` myWorkspaces) 
                  return (\w -> hidden w && empty w && knownWs w)


--adapted from http://xmonad.haskell.narkive.com/EToEJM1K/normal-rather-than-greedy-view-disable-screen-focus-switching
isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws

workspaceOfScreen :: (ScreenId) -> X (Maybe WorkspaceId)
workspaceOfScreen x =  withWindowSet $ return . W.lookupWorkspace x

getWorkspacesString screens= foldr (<+>) mempty $ DL.intersperse (return (Just " ")) [workspaceOfScreen x | x<-screens]
scWorkspaces = getOrderedScreens >>= getWorkspacesString
printWs = scWorkspaces  >>= flashText' mySTConfig 1 .fromMaybe ""

