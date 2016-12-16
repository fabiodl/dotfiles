
{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle

import XMonad.Layout.Tabbed
--import XMonad.Layout.ShowWName
--import XMonad.Actions.ShowText
import CenteredFlash

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

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Hooks.FadeInactive

import XMonad.Util.Scratchpad
import Data.Maybe (fromMaybe)
import XMonad.Util.Loggers (logCurrent)
import Data.Monoid
import XMonad.Layout.IndependentScreens

button10 = 10 :: Button
button13 = 13 :: Button


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
   
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((0,button13) , ( \w -> spawn "google-chrome" ))
    , ((0,button10) , ( \w -> spawn "gnome-terminal" )) 
    ]



myManageHook = composeAll
    [ className =? "Gimp"        --> doFloat
    , className =? "Vncviewer"   --> doFloat
    , className =? "stalonetray" --> doIgnore
    , className =? "com-mathworks-util-PostVMInit" --> doFloat
    , className =? "Cairo-dock" --> doFloat
    , className =? "platform-Emulicious" -->doFloat
    ]  <+> scratchpadManageHook (W.RationalRect 0 0 1 0.3)


myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

fadeHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.9


main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ ewmh gnomeConfig
         { logHook = do
                      -- fadeHook 
                      dynamicLogWithPP (prettyPrinter dbus) 
 , mouseBindings = myMouseBindings  
 , layoutHook = smartBorders (   mkToggle (single REFLECTX) $ layoutHook gnomeConfig) ||| tabbed shrinkText  defaultTheme
 , normalBorderColor   =  "gray50"
         , focusedBorderColor =  "skyblue"
         ,  modMask = mod4Mask -- set the mod key to the windows key
 , startupHook = setWMName "LG3D"
 , handleEventHook =
            handleEventHook gnomeConfig <+> fullscreenEventHook <+> handleTimerEvent
             ,manageHook = myManageHook <+> manageHook gnomeConfig 
            
         } `additionalKeys` myKeys

scratchPad = scratchpadSpawnActionTerminal myTerminal
myTerminal :: String
myTerminal = "urxvt"




myKeys=
 [ ((mod1Mask .|. shiftMask , xK_BackSpace), spawn "gnome-screensaver-command -l")
        , ((mod4Mask .|. shiftMask,   xK_q), spawn "xkill")
 , ((mod4Mask .|. shiftMask .|. controlMask,  xK_q), io exitSuccess)
 , ((mod4Mask , xK_g     ), windowPromptGoto  defaultXPConfig  >> printWs)
 , ((mod4Mask , xK_b     ), windowPromptBring defaultXPConfig)
 , ((mod4Mask , xK_Left),  sendToScreen 0 >> viewScreen 0 >> windows W.swapMaster)
 , ((mod4Mask , xK_Right), sendToScreen 1 >> viewScreen 1 >> windows W.swapMaster)
 , ((mod4Mask , xK_Up), sendMessage $ Toggle REFLECTX)
 , ((mod4Mask , xK_n),  XMonad.Actions.CycleWS.moveTo XMonad.Actions.CycleWS.Next XMonad.Actions.CycleWS.HiddenNonEmptyWS >> printWs)
 , ((mod4Mask .|. shiftMask, xK_n),  XMonad.Actions.CycleWS.moveTo XMonad.Actions.CycleWS.Prev XMonad.Actions.CycleWS.HiddenNonEmptyWS >> printWs)
 , ((mod4Mask .|. controlMask, xK_n),  XMonad.Actions.CycleWS.moveTo XMonad.Actions.CycleWS.Prev XMonad.Actions.CycleWS.EmptyWS >> printWs)
 , ((mod4Mask .|. shiftMask, xK_Up), swapNextScreen >> printWs)
 , ((mod4Mask , xK_i), spawn "google-chrome")
 , ((mod4Mask, xK_F12), scratchPad) -- quake terminal
 , ((mod4Mask , xK_d), spawn "gjiten")   
 , ((mod4Mask , xK_p), spawn "dmenu_run -nb black -nf skyblue -sb skyblue -sf black ") 
        ]
 ++
        [
  ((m .|. mod4Mask, k),   (windows $ f i) >> printWs) -- Replace 'mod1Mask' with your mod key of choice.
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(lazyView, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
        ]

--adapted from http://xmonad.haskell.narkive.com/EToEJM1K/normal-rather-than-greedy-view-disable-screen-focus-switching
isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws

mySTConfig = defaultSTConfig { --st_font = "xft:Droid Sans:pixelsize=28"
                          st_bg   = "black"
                         , st_fg   = "skyblue"
                         }

scWork x= withWindowSet $ return . W.lookupWorkspace x
spaced x=fmap (fmap (++" ")) x
getWorkspacesString count=foldr (<+>) mempty ([spaced (scWork  x) | x <- [0..count-2]] ++[scWork (count-1)])
scWorkspaces=  countScreens >>= getWorkspacesString
printWs= scWorkspaces  >>= flashText mySTConfig 1 .fromMaybe ""

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoColor "white" . wrap "<span font=\"Sans Bold 8\">" "</span>" .pangoSanitize
    , ppCurrent  = pangoColor "gold" . wrap "<span font=\"Sans Bold 8\">{" "}</span>" . pangoSanitize
    , ppVisible  = pangoColor "lightsalmon" . wrap "<span font=\"Sans Bold 8\">[" "]</span>" . pangoSanitize
    , ppHidden   = pangoColor "white" . wrap "<span font=\"Sans Bold 8\">(" ")</span>" . pangoSanitize . noScratchPad
    , ppUrgent   = pangoColor "red"
    , ppLayout   = pangoColor "seagreen" . wrap "<span font=\"Sans Bold 8\">|" "|</span>"
    , ppSep      = "<span font=\"Sans Bold 8\"> </span>"
    }
 where
    -- then define it down here: if the workspace is NSP then print
    -- nothing, else print it as-is
    noScratchPad ws = if ws == "NSP" then "" else ws

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

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


