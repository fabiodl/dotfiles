{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import qualified Data.Map  as M
import qualified Data.List as DL
import Data.Maybe
import Data.Function (on)
import qualified Data.Text.Lazy as TL
import Data.Time.LocalTime
import Text.Printf
import Control.Applicative((<$>))
import Numeric (showHex)
import XMonad hiding ( (|||) )
import XMonad.Config.Gnome
import XMonad.Operations as Ope

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import System.Exit

import qualified XMonad.StackSet as W

import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.LayoutCombinators 
import XMonad.Layout.Renamed
import XMonad.Layout.TwoPane
import XMonad.Layout.ComboP

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory


import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowBringer

  
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Util.Scratchpad
import XMonad.Util.XUtils(stringToPixel)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.EZConfig(additionalKeys,removeKeys)
import XMonad.Util.NamedScratchpad

import CenteredFlash
import DynamicDecoration

myModKey = mod4Mask                  

myMouseBindings (XConfig {XMonad.modMask = myModKey}) = M.fromList $ [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((myModKey, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((myModKey, button2), (\w -> focus w >> windows W.swapMaster))    
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((myModKey, button3), (\w -> focus w >> mouseResizeWindow w))

    , ((myModKey, button4), (\w ->  windows W.focusUp))
    , ((myModKey, button5), (\w ->  windows W.focusDown))
    , ((myModKey.|.controlMask, button4), (\w ->  moveTo Prev (WSIs $myHiddenWS AnyWS) >> printWs))
    , ((myModKey.|.controlMask, button5), (\w ->  moveTo Next (WSIs $myHiddenWS AnyWS) >> printWs))
    
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
    ]  -- <+> scratchpadManageHook rect
       <+> myScratchpadHook (W.RationalRect 0 0 1 0.3) ["scratchpad"]
       <+> myScratchpadHook (W.RationalRect 0 0.7 1 0.3) ["ipython"]
  where
    scratchpadQuery winNames = fmap (\x -> elem x winNames) resource --checks if the window name is in the winNames list
    myScratchpadHook rect winNames = namedScratchpadManageHook [NS "" "" (scratchpadQuery winNames) (customFloating rect)]
-- doSink = ask >>= \w -> doF (W.sink w)

myWorkspaces = map show [1..9]

myBgColor = "black"
myFgColor = "black"

--for Prompt
myXPConfig = def{
  fgColor=myFgColor
  , bgColor=myBgColor
  , fgHLight=myBgColor
  , bgHLight=myFgColor
  , borderColor=myFgColor
  , searchPredicate = \a b -> DL.isInfixOf (toUpper a) (toUpper b)  
  , alwaysHighlight = True
  }

dmenu c = "dmenu_run -nb \""++(bgColor c)++"\" -nf \""++(fgColor c)++"\" -sb \""++(fgColor c)++"\" -sf \""++(bgColor c)++"\""

wrapCharsCurrent = ("[","]")
wrapCharsVisible = ("<",">")
wrapCharsHidden  = ("(",")")
wrapCharsUrgent = ("!","!")
wrapCharsLayout = ("|","|")

type WsAction = (WorkspaceId -> WindowSet -> WindowSet )
type WithWsType = (WorkspaceId -> X () ) -> X()
type NavAction = (Direction2D -> Bool -> X())

[virtRight,virtLeft,virtUp,virtDown] = [xK_bracketright, xK_semicolon, xK_at, xK_colon]
[mvirtRight,mvirtLeft,mvirtUp,mvirtDown] = [ xK_Page_Down, xK_Delete, xK_Home, xK_End]
[virtBtn1,virtBtn3] = [xK_Insert,xK_Page_Up]  



myMouseKeys =
  [
    ((myModKey .|. m ,key), spawn $ "xdotool mousemove_relative -- " ++ show (dx* mult) ++ " " ++ show (dy* mult))
  | (m,mult) <- [(controlMask,1),(0,10),(shiftMask,100)],
    (key,dx,dy) <-[(mvirtLeft,-1,0),(mvirtRight,1,0),(mvirtUp,0,-1),(mvirtDown,0,1)]
  ]++
  [
    ((myModKey .|. m ,key), spawn $ "xdotool click " ++ show btn)
    | (key,m,btn) <- [(virtBtn1,controlMask,1),(virtBtn3,controlMask,3),(virtBtn1,shiftMask,4),(virtBtn3,shiftMask,5)]
  ]



makeTermCmds:: String -> [String] -> [(String,String,String)]
makeTermCmds setter vals = [ ("tt","term trasnp",setter++(vals!!0) ), ("th","term halftr",setter++(vals!!1) ), ("to","term opaque",setter++(vals!!2) )] 


gconfCmds=makeTermCmds "gconftool-2 --set /apps/gnome-terminal/profiles/Default/background_darkness --type=float " ["0","0.5","1"]
dconfCmds=makeTermCmds "dconf write \"/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/background-transparency-percent\" " ["100", "50", "0"] 



floatWindow :: X ()
floatWindow = withDisplay $ \display ->
  withFocused $ \window -> do
    sh <- io $ getWMNormalHints display window
    let minsize = sh_min_size sh
        maxsize = sh_max_size sh
    case (minsize, maxsize) of
      (Just (minw, minh), Just (maxw, maxh)) -> io (resizeWindow display window midw midh)
        where avg a b = ceiling $ fromIntegral (a + b) / 2
              midw = avg minw maxw
              midh = avg minh maxh
                                                                              
      _ -> return ()
    Ope.float window
  
scratchTerminal = "urxvt"
scratchpadSpawnProgram prog winName =  namedScratchpadAction [NS winName prog (resource =? winName) nonFloating] winName
scratchpadSpawnTerminalProgram prog winName = scratchpadSpawnProgram (scratchTerminal++" -name "++winName++" -e "++prog) winName

  
myKeys=
 [ ((myModKey .|. shiftMask .|. controlMask, xK_q), io exitSuccess)
 , ((myModKey                , xK_g ), printWs >> withColour myXPConfig (myWindowPrompt Goto)  >> printWs)
 , ((myModKey                , xK_b ), printWs >> withColour myXPConfig (myWindowPrompt Bring) )
 , ((myModKey                , xK_p), withColour myXPConfig{maxComplRows = Just 3} shellPrompt)
 , ((myModKey .|. shiftMask  , xK_p), withColour myXPConfig (spawn . dmenu) )
 , ((myModKey                , xK_x), quickPrompt spawnOptions)
 , ((myModKey                , xK_s), sendMessage (Toggle REFLECTX) >> printWs)
 , ((myModKey .|. shiftMask  , xK_s), screenSwap L True >>printWs)
 , ((myModKey .|. controlMask, xK_Return), sendMessage SwapWindow)
 , ((myModKey .|. shiftMask  , xK_t), sinkAll >> printWs)
 , ((myModKey                , xK_f), floatWindow)
 , ((myModKey                , xK_v), quickPrompt layoutOptions)
 , ((myModKey .|. shiftMask  , xK_BackSpace), scratchpadSpawnActionTerminal scratchTerminal) 
 , ((myModKey                , xK_BackSpace), scratchpadSpawnTerminalProgram "ipython" "ipython" ) 
 , ((myModKey .|. controlMask, xK_BackSpace), spawn "gnome-screensaver-command -l")
 , ((myModKey                , xK_F5), scratchpadSpawnProgram "slack" "slack") 
 , ((myModKey                , xK_F9), scratchpadSpawnProgram "firefox" "Navigator") 
 ]
 ++
 myMouseKeys
 ++
 makeKeybindings wsCombiner ((0,lazyView):wsActions) (withPlainWs++withHiddenWs) 
 ++
 makeKeybindings wsCombiner  ((0,W.view):wsActions) withPhysicalWs
 ++
 makeKeybindings navCombiner navActions navDirs
  where wsCombiner act ctx = ctx (windows . act) >> printWs 
        wsActions = [(shiftMask, W.shift), (mod1Mask, W.greedyView), (controlMask,shiftAndGo)] :: [(KeyMask,WsAction)]
        withPlainWs =  zip [xK_1 .. xK_9]  $ map (\ws ac -> ac ws)   myWorkspaces :: [(KeySym, WithWsType)] 
        withPhysicalWs = zip [xK_w, xK_e, xK_r] $ map (\sc ac -> screenWorkspace sc >>= flip whenJust ac) [0..] :: [(KeySym, WithWsType)] 
        withHiddenWs = [(key, doTo dir (WSIs $ myHiddenWS wsType) order) |
                           (key,dir,wsType,order) <- [ (xK_0,Next,EmptyWS,getSortByIndex)
                                                 , (xK_minus,Prev,NonEmptyWS,getSortByIndex)
                                                 , (xK_asciicircum,Next,NonEmptyWS,getSortByIndex)
                                                 , (xK_u,Next,NonEmptyWS,getSortByHistory)
                                                 ]
                     ] ::[(KeySym, WithWsType)] 
        navCombiner (act,loop) ctx = act ctx loop
        navActions = [ (0,(windowGo,False))
                     , (shiftMask,(windowSwap,False))
                     , (controlMask,(windowToScreenMaster,False))
                     , (mod1Mask,(screenGo,True))
                     ] :: [(KeyMask,(NavAction,Bool))]
        navDirs = [ (xK_Right, R), (xK_Left, L), (xK_Up, U), (xK_Down, D),
                    (virtRight, R), (virtLeft, L), (virtUp, U), (virtDown, D)
                  ]  :: [(KeySym, Direction2D)]            
        quickPrompt choices = withColour c (xmonadPromptC choices)
          where c = myXPConfig{searchPredicate = DL.isPrefixOf,  autoComplete = Just 3}
        shiftAndGo wid winset =  W.view wid $ W.shift wid winset  
        myDecorateName ws w = do name <- show <$> getName w
                                 winset <- gets windowset
                                 let tag = W.tag ws
                                     wsWindows = ((W.integrate' . W.stack) ws)
                                 wsNamedWindows <- sequence $ map getName wsWindows
                                 let wsNames = map show wsNamedWindows
                                     ncopies = length $ filter (name ==) wsNames
                                     wid = if ncopies >2 then " #" ++ showHex  w "" else ""
                                     (open,close) = if tag == W.currentTag winset then wrapCharsCurrent
                                      else if isVisible tag winset then wrapCharsVisible else wrapCharsHidden
                                 return $ open ++ W.tag ws ++ close ++ name ++ wid 
        myWindowPrompt action c = windowPrompt c action (windowMap' myDecorateName)
        windowToScreenMaster dir loop = windowToScreen dir loop >> screenGo dir loop >> windows W.swapMaster
        layoutOptions = [(key++":"++layout, sendMessage $ JumpToLayout layout) |
                           (key,layout) <- [("v","Tile"),("0","Full"),("1","Tab"),("2","Double") ]
                        ]
                        ++
                        [ ("r:Rotate",sendMessage (Toggle MIRROR))]
        spawnOptions = [(key++":"++desc,spawn cmd) |
                        (key,desc,cmd) <- [ ("d", "gjiten","gjiten")
                                          , ("e","emacs","emacs")
                                          , ("i","google-chrome","google-chrome")
                                          ,("pon","proxy on","gsettings set org.gnome.system.proxy mode 'manual'")
                                          ,("poff","proxy off","gsettings set org.gnome.system.proxy mode 'none'")
                                          ]++dconfCmds
                       ]


myDisableKeys = [((myModKey .|. shiftMask, xK_q))]

makeKeybindings :: (act->ctx->X ()) -> [(KeyMask,act)] -> [(KeySym, ctx)] -> [((KeyMask,KeySym),X())]
makeKeybindings  combiner maskAssoc keyAssoc  = [ ((myModKey .|. mask, key), combiner action context)
                                                | (mask,action) <- maskAssoc, (key,context) <- keyAssoc]

                                         

getSortByHistory :: X WorkspaceSort
getSortByHistory = mkWsSort $ do let cmp Nothing Nothing = EQ
                                     cmp Nothing (Just _) = GT
                                     cmp (Just _) Nothing = LT
                                     cmp a b = compare a b
                                 wh <- workspaceHistory   
                                 let hcmp = on cmp (\x -> DL.elemIndex x wh)                                       
                                 return hcmp



myDynamicTheme :: DynamicTheme 
myDynamicTheme = def{
  theme=do col<-clockColor
           return Theme{ activeColor         = col
                       , inactiveColor       = "black"
                       , urgentColor         = "#FFFF00"
                       , activeBorderColor   = "#FFFFFF"
                       , inactiveBorderColor = "#BBBBBB"
                       , urgentBorderColor   = "##00FF00"
                       , activeTextColor     = "black"
                       , inactiveTextColor   = col
                       , urgentTextColor     = "#FF0000"
                       , fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                       , decoWidth           = 200
                       , decoHeight          = 20
                       , windowTitleAddons   = []
                       , windowTitleIcons    = []
                       }  
  }

myTabClickBindings btn win | btn == button1 = focus win
                           | btn == button3 = focus win >> sendMessage SwapWindow
                           | otherwise    =  return ()

myTiledLayout = renamed [Replace "Tile"] $ mkToggle (single REFLECTX) $ mkToggle (single MIRROR ) $ tall 
  where tall =  Tall 1 (3/100) (1/2)  
myTabbedLayout = renamed [Replace "Tab"] $ mkToggle (single REFLECTX) $  dynamicTabs D myDynamicTheme def
myDoubleLayout = renamed [Replace "Double"] $ combineTwoP (TwoPane 0.03 0.5) tabLayout tabLayout (Const False) where
  tabLayout = dynamicTabs D myDynamicTheme def{mouseClickBindings = myTabClickBindings}
                                                                              
main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ docks $ withUrgencyHook NoUrgencyHook
           $ withNavigation2DConfig def
           $ ewmh gnomeConfig           
           { logHook = clockColor >>= (\col -> dynamicLogWithPP (pangoPP dbus col) <+> (setBorderColor col)) -- <+> fadeHook
         , mouseBindings = myMouseBindings
         , layoutHook = avoidStruts $ smartBorders  (  myTiledLayout ||| myDoubleLayout ||| myTabbedLayout ||| Full )
         , normalBorderColor   =  myBgColor
         , focusedBorderColor =  myFgColor
         , modMask = myModKey 
         , startupHook = setWMName "LG3D"
         , handleEventHook = handleEventHook gnomeConfig <+> fullscreenEventHook <+> handleTimerEvent
         , manageHook = myManageHook <+> manageHook gnomeConfig
         , workspaces = myWorkspaces
         } `additionalKeys` myKeys `removeKeys` myDisableKeys 
  
pangoPP :: D.Client -> String -> PP
pangoPP dbus col = def
    { ppOutput   = dbusOutput dbus
    , ppTitle    = myFormat "white" sans 
    , ppTitleSanitize = pangoSanitize
    , ppCurrent  = myFormat "gold" mono .  pangoSanitize . wrapBy wrapCharsCurrent
    , ppVisible  = myFormat "darkorange" mono . pangoSanitize . wrapBy wrapCharsVisible
    , ppHidden   = myFormat "white" mono . pangoSanitize . wrapBy wrapCharsHidden . onlyKnown
    , ppUrgent   = myFormat "red"  mono . pangoSanitize .wrapBy wrapCharsUrgent
    , ppLayout   = myFormat col mono . pangoSanitize . wrapBy wrapCharsLayout
    , ppSep      = pangoFont mono " "
    }
 where
    onlyKnown ws = if ws `elem` myWorkspaces then ws else "" --successive wraps return "" for a "" argument
    sans = "Sans Bold 8"
    mono = "Monospace Bold 8" 
    myFormat color font = wrap ("<span foreground=\"" ++color++"\" font=\""++font++"\">") "</span>"

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

pangoFont :: String -> String -> String
pangoFont font = wrap left right
  where
    left="<span font=\""++font++"\">"
    right="</span>"

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

wrapBy :: (String,String) -> String -> String
wrapBy (open,close) = wrap open close

cmpScreen' :: Rectangle -> Rectangle -> Ordering
cmpScreen' (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (y1,x1) (y2,x2)

getOrderedScreens :: X [ScreenId]
getOrderedScreens =do w <- gets windowset
                      return (map W.screen $ DL.sortBy (cmpScreen' `on` (screenRect . W.screenDetail)) $ W.current w : W.visible w)

myHiddenWS :: WSType -> X (WindowSpace -> Bool)
myHiddenWS t= do hs <- gets (map W.tag . W.hidden . windowset)
                 let e = case t of
                      EmptyWS-> isNothing . W.stack
                      NonEmptyWS -> not .isNothing . W.stack
                      _ -> (\_ -> True)

                 let hidden = (\w -> W.tag w `elem` hs)
                 let knownWs = (\w -> W.tag w `elem` myWorkspaces) 
                 return (\w -> hidden w && e w && knownWs w)

--adapted from http://xmonad.haskell.narkive.com/EToEJM1K/normal-rather-than-greedy-view-disable-screen-focus-switching
isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)

lazyView :: WorkspaceId -> WindowSet -> WindowSet
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
                         return $ foldr (++) "" (DL.intersperse " " [ highlight (W.currentTag w)  (fromMaybe "" $ W.lookupWorkspace sc w)  | sc <- screens])
                         where highlight curr ws = if curr == ws then wrapBy wrapCharsCurrent ws  else ws
                         
withColour :: XPConfig -> (XPConfig -> X() ) -> X()
withColour c f = do col <-clockColor
                    f c{ fgColor = col
                       , bgHLight = col
                       , borderColor = col
                       }

printWs :: X ()
printWs = do text <- getWorkspacesString
             c <-clockColor
             flashText' def{st_bg = myBgColor, st_fg = c} 1 text

clockColor :: X String
clockColor = do now<-io getTime
                return (timeToColor now)

timeToColor :: TimeOfDay-> String
timeToColor time = let maxTime = 3600.0*23.0+60.0*59+61.0  :: Float
                       theta = 2*2*pi/maxTime *(  3600.0*(fromIntegral $ todHour time ) --2 cycles a day
                                              +  60.0* (fromIntegral $ todMin time)
                                              + realToFrac(todSec time))   :: Float
                       (c,s) = (cos(theta),sin(theta))
                       hue = clip 0 1 $ -0.363*c+0.101*s+0.379 
                       sat = clip 0 mxs $ 0.100*c+0.360*s+0.758 where
                         mxs = min 1 $ 0.6+0.4*abs(hue-(2.0/3.0)) 
                       val = clip 0 1$ 0.014*c+0.050*s+0.966 
                       color = hsv' hue sat val
                       [r,g,b]= map (clip 0 255 . round . (*255)) color ::[Integer]
                   in printf "#%02X%02X%02X" r g b where clip minv maxv x = min maxv $ max minv x
                                                         
getTime :: IO TimeOfDay                      
getTime = fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime

--takes hue values in the range 0-1
hsv' :: (RealFrac a, Ord a) => a -> a -> a -> [a]
hsv' h s v = case hi of
    0 -> [v,t,p]
    1 -> [q,v,p]
    2 -> [p,v,t]
    3 -> [p,q,v]
    4 -> [t,p,v]
    5 -> [v,p,q]
 where
  hi = floor (6*h) `mod` 6
  f = mod1 (6*h)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

mod1 x | pf < 0 = pf+1
       | otherwise = pf
 where
  (_,pf) = properFraction x

