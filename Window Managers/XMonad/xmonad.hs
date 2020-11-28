-- Base
import XMonad
import Data.Ratio 
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce



import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.SpawnOnce
import System.IO
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Maximize
--import XMonad.Layout.BoringWindows
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Actions.Minimize
import XMonad.Hooks.Minimize
import qualified Data.Map                            as M
import qualified XMonad.StackSet                     as W
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppLauncher
import XMonad.Prompt.Man
import XMonad.Prompt.FuzzyMatch
import Data.Char (isSpace, toUpper)
import Control.Arrow (first)
import Data.List (isPrefixOf, nub)
import Data.List.Split
import System.Process
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Layout.Renamed
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Hooks.InsertPosition
import qualified XMonad.Actions.CycleWS as CWS

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

myBrowser :: String
myBrowser = "firefox"

myFont :: String
myFont = "xft:Noto Sans:size=10"

-- XPrompt
-- The layout hook
myLayoutHook = minimize $ maximizeWithPadding 0 $ smartBorders $ avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder 2 $ tall
                                 ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| noBorders tabs
                                 ||| grid
                                 ||| spirals
                                 ||| threeCol
                                 ||| threeRow


-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 12
           $ mySpacing 2
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 12
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (noBorders Simplest)
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }


scratchpads = [
    NS "Tmux" "st -c Tmux -e tmux" (className =? "Tmux") nonFloating ,
    NS "FileManager" "st -c FileManager -e nnn -x" (className =? "FileManager") (doRectFloat (W.RationalRect (3 % 4) (1 % 8) (1 % 4) (3 % 4))) ,
    NS "Org" "emacs --config org" (title =? "Org") nonFloating ,
    NS "News" "emacs --config news" (title =? "News") nonFloating ,
    NS "Mail" "emacs --config mail" (title =? "Mail") nonFloating ,
    NS "Tracking" "emacs --config tracking" (title =? "Tracking") nonFloating
    ]
runXPConfig :: XPConfig
runXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      -- , position            = Top
      , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 30
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , defaultPrompter     = id
      , alwaysHighlight     = True
      , maxComplRows        = Just 10      -- set to 'Just 5' for 5 rows
      }

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     , (xK_n, moveHistory W.focusUp')
     , (xK_p, moveHistory W.focusDown')
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Escape, quit)
     ]

xKill::Window -> X()
xKill w = withDisplay $ \d -> do
    wmdelt <- atom_WM_DELETE_WINDOW  ;  wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols d w
    io $ if wmdelt `elem` protocols
    then killClient d w >> return ()
        else killClient d w >> return ()

myManageHook = composeAll
	[ className =? "Surf"   --> doRectFloat (W.RationalRect (1 % 16) (1 % 16) (7 % 8) (7 % 8))
	, className =? "tabbed"   --> doRectFloat (W.RationalRect (1 % 8) (1 % 8) (5 % 8) (3 % 4))
	, appName =? "Webcam"   --> doFloat
	, namedScratchpadManageHook scratchpads 
	]
myHandleEventHook = minimizeEventHook


browse a = spawn (myBrowser ++ " \"" ++ a ++ "\"")

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $  ewmh $ defaultConfig
        { manageHook = insertPosition Master Newer <+> myManageHook <+> manageHook defaultConfig 
        , layoutHook = myLayoutHook
	, terminal = "st"
	, normalBorderColor = "#001100"
	, focusedBorderColor = "#00ff00"
	, handleEventHook = myHandleEventHook  
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        } 
        , modMask = mod4Mask
	, startupHook = do
			spawnOnce "sh ~/.config/sxhkd/scripts/session_start.sh"
        } `additionalKeysP`
        [ ("M-S-r" , spawn "xmonad --recompile && xmonad --restart")
        , ("M-S-<Return>", namedScratchpadAction scratchpads "Tmux")
        , ("M-S-f", do
	namedScratchpadAction scratchpads "FileManager"
	spawn "sh ~/.config/sxhkd/scripts/xdotoggle.sh tabbed")
        , ("M-d o", namedScratchpadAction scratchpads "Org")
        , ("M-d n", namedScratchpadAction scratchpads "News")
        , ("M-d t", namedScratchpadAction scratchpads "Tracking")
        , ("M-d m", namedScratchpadAction scratchpads "Mail")
        , ("M-x" , withFocused (sendMessage . maximizeRestore))
        , ("M-c" , kill)
        , ("M-S-c" , withFocused xKill)
	--spawn "xkill -id $(xdo id)")
        , ("C-<Print>", spawn "sleep 0.2; scrot -s")
        , ("<Print>", spawn "scrot")
       , ("M-z", withFocused minimizeWindow)
       , ("M-S-z",withLastMinimized maximizeWindowAndFocus)
       , ("M-<Page_Up>", spawn "picom-trans -c +5")
       , ("M-<Page_Down>", spawn "picom-trans -c -5")
       , ("M-t", withFocused toggleFloat)
       , ("M-p", spawn "rofi -show run -theme Pop-Dark -font 'Noto Sans 14'")
       , ("M-S-p", spawn "rofi -show drun -theme Pop-Dark -font 'Noto Sans 14'")
       , ("M-r m", manPrompt runXPConfig)
       , ("M-f f", spawn "sh ~/.config/sxhkd/scripts/bookmarks.sh")
       -- , ("M-r f", inputPromptWithCompl runXPConfig "Finance"
       --              ( mkComplFunFromList' financeWebsites ) ?+ browse)
       , ("M-r q", spawn "~/.emacs.d/scripts/quote")
       , ("M-d x", xmonadPrompt runXPConfig)
       , ("M-S-w", windowPrompt runXPConfig Bring allWindows)
       , ("M-w", windowPrompt runXPConfig Goto allWindows)
       , ("M-b", sendMessage $ MT.Toggle NOBORDERS)
       , ("M-C-h", sendMessage $ pullGroup L)
       , ("M-C-l", sendMessage $ pullGroup R)
       , ("M-C-k", sendMessage $ pullGroup U)
       , ("M-C-j", sendMessage $ pullGroup D)

       , ("M-C-m", withFocused (sendMessage . MergeAll))
       , ("M-C-u", withFocused (sendMessage . UnMerge))
       , ("M-<Right>",  CWS.nextWS)
       , ("M-<Left>",  CWS.prevWS)
       , ("M-S-<Right>",  CWS.shiftToNext >> CWS.nextWS)
       , ("M-S-<Left>",   CWS.shiftToPrev >> CWS.prevWS)
       , ("M-`",      CWS.toggleWS)
       , ("M-C-.", onGroup W.focusUp')
       , ("M-C-,", onGroup W.focusDown')
       ,("M-C-b", sendMessage $ ToggleStrut U)
       , ("<XF86AudioRaiseVolume>", spawn "sh ~/.config/sxhkd/scripts/volume.sh increase")
       , ("<XF86AudioLowerVolume>", spawn "sh ~/.config/sxhkd/scripts/volume.sh decrease")
       , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 100-")
       , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +100")
       , ("<XF86Tools>", spawn "redshift -O 3500K")
       , ("M-<XF86Tools>", spawn "redshift -x")
       , ("<XF86AudioMute>", spawn "pactl list sinks | grep -q Mute:.no && pactl set-sink-mute 0 1 || pactl set-sink-mute 0 0")
       , ("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle")
       , ("M-<XF86Favorites>", spawn "sh /home/arya/.local/scripts/webcam.sh")

        ]
       	where
		toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
