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
import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import XMonad.Hooks.Minimize
import qualified Data.Map                            as M
import qualified XMonad.StackSet                     as W
myLayout = minimize(maximizeWithPadding 0 (avoidStruts (Tall 1 (3/100) (1/2) ||| Full)))
scratchpads = [
    NS "Tmux" "st -c Tmux -e tmux" (className =? "Tmux") nonFloating ,
    NS "Org" "emacs --config org" (title =? "Org") nonFloating ,
    NS "News" "emacs --config news" (title =? "News") nonFloating ,
    NS "Mail" "emacs --config mail" (title =? "Mail") nonFloating ,
    NS "Tracking" "emacs --config tracking" (title =? "Tracking") nonFloating
    ]

myManageHook = namedScratchpadManageHook scratchpads 
myHandleEventHook = minimizeEventHook
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ ewmh defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig -- make sure to include myManageHook definition from above
        , layoutHook = myLayout
	, terminal = "st"
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
        , ("M-e o", namedScratchpadAction scratchpads "Org")
        , ("M-e n", namedScratchpadAction scratchpads "News")
        , ("M-e t", namedScratchpadAction scratchpads "Tracking")
        , ("M-e m", namedScratchpadAction scratchpads "Mail")
        , ("M-d w", spawn "sh ~/.config/sxhkd/scripts/windows.sh")
        , ("M-x" , withFocused (sendMessage . maximizeRestore))
        , ("M-c" , kill)
        , ("M-S-c" , spawn "xkill -id $(xdo id)")
        , ("C-<Print>", spawn "sleep 0.2; scrot -s")
        , ("<Print>", spawn "scrot")
       , ("M-z", withFocused minimizeWindow)
       , ("M-S-z", withLastMinimized maximizeWindowAndFocus)
       , ("M-t", withFocused toggleFloat)

        ]
       	where
		toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
