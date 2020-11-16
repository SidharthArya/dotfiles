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


myLayout = maximizeWithPadding 0 (Tall 1 (3/100) (1/2) ||| Full)
scratchpads = [
    NS "Tmux" "st -c Tmux -e tmux" (className =? "Tmux") nonFloating ,
    NS "Org" "emacs --config org" (title =? "Org") nonFloating ,
    NS "News" "emacs --config news" (title =? "News") nonFloating ,
    NS "Mail" "emacs --config mail" (title =? "Mail") nonFloating ,
    NS "Tracking" "emacs --config tracking" (title =? "Tracking") nonFloating
    ]

myManageHook = namedScratchpadManageHook scratchpads 

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ ewmh defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig -- make sure to include myManageHook definition from above
        , layoutHook = avoidStruts myLayout
	, terminal = "st"
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
        , ("M-x" , withFocused (sendMessage . maximizeRestore))
        , ("C-<Print>", spawn "sleep 0.2; scrot -s")
        , ("<Print>", spawn "scrot")
        ]
