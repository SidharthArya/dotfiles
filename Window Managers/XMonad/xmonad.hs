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
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import XMonad.Hooks.Minimize
import qualified Data.Map                            as M
import qualified XMonad.StackSet                     as W
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.FuzzyMatch
import Data.Char (isSpace, toUpper)
import Control.Arrow (first)
import Data.List (isPrefixOf, nub)
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts


myFont :: String
myFont = "xft:Noto Sans:size=10"


myLayout = (minimize (maximizeWithPadding 0 (avoidStruts  (Tall 1 (3/100) (1/2) ||| Full))))
scratchpads = [
    NS "Tmux" "st -c Tmux -e tmux" (className =? "Tmux") nonFloating ,
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
      , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
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
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

xKill::Window -> X()
xKill w = withDisplay $ \d -> do
    wmdelt <- atom_WM_DELETE_WINDOW  ;  wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols d w
    io $ if wmdelt `elem` protocols
    then killClient d w >> return ()
        else killClient d w >> return ()

myManageHook = namedScratchpadManageHook scratchpads 
myHandleEventHook = minimizeEventHook

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $  ewmh $ defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig -- make sure to include myManageHook definition from above
        , layoutHook = myLayout
	, terminal = "st"
	, borderWidth = 6
	, normalBorderColor = "#001100"
	, focusedBorderColor = "#006600"
	, handleEventHook = myHandleEventHook  
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        } 
        , modMask = mod4Mask
	, startupHook = do
			spawnOnce "sh ~/.config/sxhkd/scripts/session_start.sh"
        } `additionalKeysP`
        [ ("M-S-z" , spawn "xscreensaver-command -lock")
        , ("M-S-r" , spawn "xmonad --recompile && xmonad --restart")
        , ("M-S-<Return>", namedScratchpadAction scratchpads "Tmux")
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
       , ("M-p", shellPrompt runXPConfig)
       , ("M-r m", manPrompt runXPConfig)
       , ("M-r q", spawn "~/.emacs.d/scripts/quote")
       , ("M-d x", xmonadPrompt runXPConfig)
       , ("M-d S-w", windowPrompt runXPConfig Bring allWindows)
       , ("M-d w", windowPrompt runXPConfig Goto allWindows)
        ]
       	where
		toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
