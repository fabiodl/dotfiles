{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  DynamicDecoration
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier and a class for easily creating decorated
-- layouts.
-----------------------------------------------------------------------------

module DynamicDecoration
    (
      dynamicDecoration
    , DynamicTheme (..), def
    , DynamicDecoration
    , dynamicTabs
    , Direction2D(..)
    ,  Theme (..)
    ) where

import Control.Monad (when)
import Data.Maybe
import Data.List
import Foreign.C.Types(CInt)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), diff, listFromList)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Invisible
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Util.Image
import XMonad.Layout.Decoration hiding (Decoration (..), DecorationState, OrigWin, DecorationStyle (..),decoration ,findWindowByDecoration )

import XMonad.Util.Types (Direction2D(..))
import XMonad.Layout.Simplest
-- $usage
-- This module is intended for layout developers, who want to decorate
-- their layouts. End users will not find here very much for them.
--
-- For examples of 'DecorationStyle' instances you can have a look at
-- "XMonad.Layout.SimpleDecoration", "XMonad.Layout.Tabbed",
-- "XMonad.Layout.DwmStyle", or "XMonad.Layout.TabBarDecoration".

-- | A layout modifier that, with a 'Shrinker', a 'Theme', a
-- 'DecorationStyle', and a layout, will decorate this layout
-- according to the decoration style provided.
--
-- For some usage examples see "XMonad.Layout.DecorationMadness".

-- | A 'Theme' is a record of colors, font etc., to customize a
-- 'DecorationStyle'.

dynamicDecoration :: (DecorationStyle ds a, Shrinker s) => s -> DynamicTheme -> ds a
           -> l a -> ModifiedLayout (DynamicDecoration ds s) l a
dynamicDecoration s t ds = ModifiedLayout (DynamicDecoration (I Nothing) s t ds)

dynamicTabs location theme = dynamicDecoration shrinkText theme (Tabbed  location Always) Simplest 


newtype DynamicTheme = DynamicTheme { theme:: X Theme}

instance Show (DynamicTheme) where show _ = ""

instance Read (DynamicTheme) where readsPrec _ s = [(def,s)]
instance Default DynamicTheme where
  def = DynamicTheme {theme=return def}



-- | The 'Decoration' state component, where the list of decorated
-- window's is zipped with a list of decoration. A list of decoration
-- is a list of tuples, a 'Maybe' 'Window' and a 'Maybe Rectangle'.
-- The 'Window' will be displayed only if the rectangle is of type
-- 'Just'.
data DecorationState =
    DS { decos :: [(OrigWin,DecoWin)]
       , font  :: XMonadFont
       }
type DecoWin = (Maybe Window, Maybe Rectangle)
type OrigWin = (Window,Rectangle)

-- | The 'Decoration' 'LayoutModifier'. This data type is an instance
-- of the 'LayoutModifier' class. This data type will be passed,
-- together with a layout, to the 'ModifiedLayout' type constructor
-- to modify the layout by adding decorations according to a
-- 'DecorationStyle'.
data DynamicDecoration ds s a =
    DynamicDecoration (Invisible Maybe DecorationState) s DynamicTheme (ds a)
    deriving (Show, Read)

-- | The 'DecorationStyle' class, defines methods used in the
-- implementation of the 'Decoration' 'LayoutModifier' instance. A
-- type instance of this class is passed to the 'Decoration' type in
-- order to decorate a layout, by using these methods.
-- | The default 'DecorationStyle', with just the default methods'
-- implementations.

-- | The long 'LayoutModifier' instance for the 'Decoration' type.
--
-- In 'redoLayout' we check the state: if there is no state we
-- initialize it.
--
-- The state is 'diff'ed against the list of windows produced by the
-- underlying layout: removed windows get deleted and new ones
-- decorated by 'createDecos', which will call 'decorate' to decide if
-- a window must be given a 'Rectangle', in which case a decoration
-- window will be created.
--
-- After that we resync the updated state with the windows' list and
-- then we process the resynced stated (as we do with a new state).
--
-- First we map the decoration windows, we update each decoration to
-- reflect any decorated window's change, and we insert, in the list
-- of windows and rectangles returned by the underlying layout, the
-- decoration for each window. This way xmonad will restack the
-- decorations and their windows accordingly. At the end we remove
-- invisible\/stacked windows.
--
-- Message handling is quite simple: when needed we release the state
-- component of the 'Decoration' 'LayoutModifier'. Otherwise we call
-- 'handleEvent', which will call the appropriate 'DecorationStyle'
-- methods to perform its tasks.
instance (DecorationStyle ds Window, Shrinker s) => LayoutModifier (DynamicDecoration ds s) Window where
    redoLayout (DynamicDecoration (I (Just s)) sh t ds) _ Nothing _ = do
        releaseResources s
        return ([], Just $ DynamicDecoration (I Nothing) sh t ds)
    redoLayout _                                 _ Nothing _  = return ([], Nothing)

    redoLayout (DynamicDecoration st sh dynt ds) sc (Just stack) wrs
        | I Nothing  <- st = do t <- theme dynt
                                initState t ds sc stack wrs >>= (\s ->processState s t)
        | I (Just s) <- st = do t<- theme dynt
                                let dwrs  = decos s
                                    (d,a) = curry diff (get_ws dwrs) ws
                                    toDel = todel d dwrs
                                    toAdd = toadd a wrs
                                deleteDecos (map snd toDel)
                                let ndwrs = zip toAdd $ repeat (Nothing,Nothing)
                                ndecos <- resync (ndwrs ++ del_dwrs d dwrs) wrs t
                                processState (s {decos = ndecos }) t
        | otherwise        = return (wrs, Nothing)

        where
          ws        = map fst wrs
          get_w     = fst . fst
          get_ws    = map get_w
          del_dwrs  = listFromList get_w notElem
          find_dw i = fst . snd . flip (!!) i
          todel   d = filter (flip elem d . get_w)
          toadd   a = filter (flip elem a . fst  )

          check_dwr dwr t= case dwr of
                            (Nothing, Just dr) -> do dw <- createDecoWindow t dr
                                                     return (Just dw, Just dr)
                            _                 -> return dwr

          resync _         [] t = return []
          resync d ((w,r):xs) t = case  w `elemIndex` get_ws d of
                                  Just i  -> do dr   <- decorate ds (decoWidth t) (decoHeight t) sc stack wrs (w,r)
                                                dwr  <- check_dwr (find_dw i d, dr) t
                                                dwrs <- resync d xs t
                                                return $ ((w,r),dwr) : dwrs
                                  Nothing -> resync d xs t

          -- We drop any windows that are *precisely* stacked underneath
          -- another window: these must be intended to be tabbed!
          remove_stacked rs ((w,r):xs)
              | r `elem` rs   = remove_stacked rs xs
              | otherwise     = (w,r) : remove_stacked (r:rs) xs
          remove_stacked _ [] = []

          insert_dwr ((w,r),(Just dw,Just dr)) xs = (dw,dr):(w, shrink ds dr r):xs
          insert_dwr (x    ,(     _ ,     _ )) xs = x:xs

          dwrs_to_wrs    = remove_stacked [] . foldr insert_dwr []

          processState s t = do let ndwrs = decos s
                                showDecos (map snd ndwrs)
                                updateDecos sh t (font s) ndwrs
                                return (dwrs_to_wrs ndwrs, Just (DynamicDecoration (I (Just (s {decos = ndwrs}))) sh dynt ds))

    handleMess (DynamicDecoration (I (Just s@(DS {decos = dwrs}))) sh dynt ds) m
        | Just e <- fromMessage m                = do t<- theme dynt
                                                      decorationEventHook ds s e
                                                      -- the following causes a redraw when typing or moving the mouse
                                                      -- handleEvent sh t s e 
                                                      return Nothing
        | Just Hide             <- fromMessage m = do hideDecos (map snd dwrs)
                                                      return Nothing
        | Just ReleaseResources <- fromMessage m = do t<- theme dynt
                                                      releaseResources s
                                                      return $ Just $ DynamicDecoration (I Nothing) sh dynt  ds
    handleMess _ _ = return Nothing

    modifierDescription (DynamicDecoration _ _ _ ds) = describeDeco ds

-- | By default 'Decoration' handles 'PropertyEvent' and 'ExposeEvent'
-- only.
handleEvent :: Shrinker s => s -> Theme -> DecorationState -> Event -> X ()
handleEvent sh t (DS dwrs fs) e
    | PropertyEvent {ev_window = w} <- e
    , Just i <- w `elemIndex`             (map (fst . fst) dwrs) = updateDeco sh t fs (dwrs !! i)
    | ExposeEvent   {ev_window = w} <- e
    , Just i <- w `elemIndex` (catMaybes $ map (fst . snd) dwrs) = updateDeco sh t fs (dwrs !! i)
handleEvent _ _ _ _ = return ()

-- | Mouse focus and mouse drag are handled by the same function, this
-- way we can start dragging unfocused windows too.
handleMouseFocusDrag :: (DecorationStyle ds a) => ds a -> DecorationState -> Event -> X ()
handleMouseFocusDrag ds (DS dwrs _) ButtonEvent { ev_window     = ew
                                                , ev_event_type = et
                                                , ev_x_root     = ex
                                                , ev_y_root     = ey }
    | et == buttonPress
    , Just ((mainw,r), (_, decoRectM)) <- lookFor ew dwrs = do
        let Just (Rectangle dx _ dwh _) = decoRectM
            distFromLeft = ex - fi dx
            distFromRight = fi dwh - (ex - fi dx)
        dealtWith <- decorationCatchClicksHook ds mainw (fi distFromLeft) (fi distFromRight)
        when (not dealtWith) $ do
            mouseDrag (\x y -> focus mainw >> decorationWhileDraggingHook ds ex ey (mainw, r) x y)
                        (decorationAfterDraggingHook ds (mainw, r) ew)
handleMouseFocusDrag _ _ _ = return ()

handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleDraggingInProgress ex ey (_, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ SetGeometry rect

-- | Given a window and the state, if a matching decoration is in the
-- state return it with its ('Maybe') 'Rectangle'.
lookFor :: Window -> [(OrigWin,DecoWin)] -> Maybe (OrigWin,(Window,Maybe Rectangle))
lookFor w ((wr,(Just dw,dr)):dwrs) | w == dw = Just (wr,(dw,dr))
                                   | otherwise = lookFor w dwrs
lookFor w ((_, (Nothing, _)):dwrs) = lookFor w dwrs
lookFor _ [] = Nothing


-- | Initialize the 'DecorationState' by initializing the font
-- structure and by creating the needed decorations.
initState :: DecorationStyle ds Window => Theme -> ds Window -> Rectangle
          -> W.Stack Window -> [(Window,Rectangle)] -> X DecorationState
initState t ds sc s wrs = do
  fs   <- initXMF (fontName t)
  dwrs <- createDecos t ds sc s wrs wrs
  return $ DS dwrs fs

-- | Delete windows stored in the state and release the font structure.
releaseResources :: DecorationState -> X ()
releaseResources s = do
  deleteDecos (map snd $ decos s)
  releaseXMF  (font s)

-- | Create the decoration windows of a list of windows and their
-- rectangles, by calling the 'decorate' method of the
-- 'DecorationStyle' received.
createDecos :: DecorationStyle ds Window => Theme -> ds Window -> Rectangle -> W.Stack Window
            -> [(Window,Rectangle)] -> [(Window,Rectangle)] -> X [(OrigWin,DecoWin)]
createDecos t ds sc s wrs ((w,r):xs) = do
  deco <- decorate ds (decoWidth t) (decoHeight t) sc s wrs (w,r)
  case deco of
    Just dr -> do dw   <- createDecoWindow t dr
                  dwrs <- createDecos t ds sc s wrs xs
                  return $ ((w,r), (Just dw, Just dr)) : dwrs
    Nothing -> do dwrs <- createDecos t ds sc s wrs xs
                  return $ ((w,r), (Nothing, Nothing)) : dwrs
createDecos _ _ _ _ _ [] = return []

createDecoWindow :: Theme -> Rectangle -> X Window
createDecoWindow t r = let mask = Just (exposureMask .|. buttonPressMask) in
                       createNewWindow r mask (inactiveColor t) True

showDecos :: [DecoWin] -> X ()
showDecos = showWindows . catMaybes . map fst . filter (isJust . snd)

hideDecos :: [DecoWin] -> X ()
hideDecos = hideWindows . catMaybes . map fst

deleteDecos :: [DecoWin] -> X ()
deleteDecos = deleteWindows . catMaybes . map fst

updateDecos :: Shrinker s => s -> Theme -> XMonadFont -> [(OrigWin,DecoWin)] -> X ()
updateDecos s t f = mapM_ $ updateDeco s t f

-- | Update a decoration window given a shrinker, a theme, the font
-- structure and the needed 'Rectangle's
updateDeco :: Shrinker s => s -> Theme -> XMonadFont -> (OrigWin,DecoWin) -> X ()
updateDeco sh t fs ((w,_),(Just dw,Just (Rectangle _ _ wh ht))) = do
  nw  <- getName w
  ur  <- readUrgents
  dpy <- asks display
  let focusColor win ic ac uc = (maybe ic (\focusw -> case () of
                                                       _ | focusw == win -> ac
                                                         | win `elem` ur -> uc
                                                         | otherwise     -> ic) . W.peek)
                                `fmap` gets windowset
  (bc,borderc,tc) <- focusColor w (inactiveColor t, inactiveBorderColor t, inactiveTextColor t)
                                  (activeColor   t, activeBorderColor   t, activeTextColor   t)
                                  (urgentColor   t, urgentBorderColor   t, urgentTextColor   t)
  let s = shrinkIt sh
  name <- shrinkWhile s (\n -> do size <- io $ textWidthXMF dpy fs n
                                  return $ size > fromIntegral wh - fromIntegral (wh `div` 10)) (show nw)
  let als = AlignCenter : map snd (windowTitleAddons t)
      strs = name : map fst (windowTitleAddons t)
      i_als = map snd (windowTitleIcons t)
      icons = map fst (windowTitleIcons t)
  paintTextAndIcons dw fs wh ht 1 bc borderc tc bc als strs i_als icons
updateDeco _ _ _ (_,(Just w,Nothing)) = hideWindow w
updateDeco _ _ _ _ = return ()

--- THE following is copyed because XMonad.Layout.Decoration.DecorationState constructor is not accessible
findWindowByDecoration :: Window -> DecorationState -> Maybe (OrigWin,(Window,Maybe Rectangle))
findWindowByDecoration w ds = lookFor w (decos ds)

class (Read (ds a), Show (ds a), Eq a) => DecorationStyle ds a where

    -- | The description that the 'Decoration' modifier will display.
    describeDeco :: ds a -> String
    describeDeco ds = show ds

    -- | Shrink the window's rectangle when applying a decoration.
    shrink :: ds a -> Rectangle -> Rectangle -> Rectangle
    shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    -- | The decoration event hook
    decorationEventHook :: ds a -> DecorationState -> Event -> X ()
    decorationEventHook ds s e = handleMouseFocusDrag ds s e

    -- | A hook that can be used to catch the cases when the user
    -- clicks on the decoration. If you return True here, the click event
    -- will be considered as dealt with and no further processing will take place.
    decorationCatchClicksHook :: ds a
                              -> Window
                              -> Int    -- ^ distance from the left where the click happened on the decoration
                              -> Int    -- ^ distance from the right where the click happened on the decoration
                              -> X Bool
    decorationCatchClicksHook _ _ _ _ = return False

    -- | This hook is called while a window is dragged using the decoration.
    -- The hook can be overwritten if a different way of handling the dragging
    -- is required.
    decorationWhileDraggingHook :: ds a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleDraggingInProgress ex ey (mainw, r) x y

    -- | This hoook is called after a window has been dragged using the decoration.
    decorationAfterDraggingHook :: ds a -> (Window, Rectangle) -> Window -> X ()
    decorationAfterDraggingHook _ds (mainw, _r) _decoWin = focus mainw

    -- | The pure version of the main method, 'decorate'.
    pureDecoration :: ds a -> Dimension -> Dimension -> Rectangle
                   -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> Maybe Rectangle
    pureDecoration _ _ ht _ s _ (w,Rectangle x y wh ht') = if isInStack s w && (ht < ht')
                                                             then Just $ Rectangle x y wh ht
                                                             else Nothing

    -- | Given the theme's decoration width and height, the screen
    -- rectangle, the windows stack, the list of windows and
    -- rectangles returned by the underlying layout and window to be
    -- decorated, tupled with its rectangle, produce a 'Just'
    -- 'Rectangle' or 'Nothing' if the window is not to be decorated.
    decorate :: ds a -> Dimension -> Dimension -> Rectangle
             -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> X (Maybe Rectangle)
    decorate ds w h r s wrs wr = return $ pureDecoration ds w h r s wrs wr


data TabbarShown = Always | WhenPlural deriving (Read, Show, Eq)

data TabbedDecoration a = Tabbed Direction2D TabbarShown deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco (Tabbed U _ ) = "Tabbed"
    describeDeco (Tabbed D _ ) = "Tabbed Bottom"
    describeDeco (Tabbed L _ ) = "Tabbed Left"
    describeDeco (Tabbed R _ ) = "Tabbed Right"
    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <- findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration (Tabbed lc sh) wt ht _ s wrs (w,r@(Rectangle x y wh hh))
        = if ((sh == Always && numWindows > 0) || numWindows > 1)
          then Just $ case lc of
                        U -> upperTab
                        D -> lowerTab
                        L -> leftTab
                        R -> rightTab
          else Nothing
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (W.integrate s)
              loc k h i = k + fi ((h * fi i) `div` max 1 (fi $ length ws))
              esize k h = fi $ maybe k (\i -> loc k h (i+1) - loc k h i) $ w `elemIndex` ws
              wid = esize x wh
              hid = esize y hh
              n k h = maybe k (loc k h) $ w `elemIndex` ws
              nx = n x wh
              ny = n y hh
              upperTab = Rectangle nx  y wid (fi ht)
              lowerTab = Rectangle nx (y + fi (hh - ht)) wid (fi ht)
              leftTab = Rectangle x ny (fi wt) hid
              rightTab = Rectangle (x + fi (wh - wt)) ny (fi wt) hid
              numWindows = length ws
    shrink (Tabbed loc _ ) (Rectangle _ _ dw dh) (Rectangle x y w h)
        = case loc of
            U -> Rectangle x (y + fi dh) w (h - dh)
            D -> Rectangle x y w (h - dh)
            L -> Rectangle (x + fi dw) y (w - dw) h
            R -> Rectangle x y (w - dw) h



