-- Haskell libraries
import Data.Ratio                   ((%))
import System.IO
-- XMonad
import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- Layout
import XMonad.Layout                
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Tabbed
-- Utils
import XMonad.Util.Font
import XMonad.Util.Run              (spawnPipe)
import XMonad.Util.Loggers


main = do 
        bar <- myBar
        xmonad $ defaultConfig
            { terminal           = myTerminal
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , workspaces         = myWorkspaces
            , manageHook         = myManageHook
            , layoutHook         = myLayoutHook
            , logHook            = dynamicLogWithPP $ myXmobar bar
            , modMask            = myModMask
            }

--
-- xmonad 
--
myTerminal   = "xterm"
myWorkspaces = ["main", "web", "dev", "test", "social", "media", "extra"]
myModMask    = mod4Mask     -- Win or Cmd key


--
-- aesthetics
--
myBorderWidth        = 2
myNormalBorderColor  = "#CCCCCC"
myFocusedBorderColor = "#FF0099"

--
-- status bar
--
myBar = spawnPipe "xmobar"

myXmobar bar = defaultPP
                    { ppCurrent = xmobarColor "yellow" "" . wrap "|" "|"
                    , ppHidden  = xmobarColor "gray" ""
                    , ppSep     = " - "
                    , ppWsSep   = "·"
                    , ppTitle   = xmobarColor "white" "" . wrap "<" ">" . shorten 50
                    , ppLayout  = xmobarColor "green" "" . wrap "_" "_"
                    , ppOrder   = \(ws:l:t:xs) -> [ws,l,t] ++ xs
                    , ppOutput  = hPutStrLn bar
                    }


-- 
-- layouts
--
myLayoutHook = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| Full ||| Accordion
            where
                tiled         = Tall masterWindows delta ratio
                masterWindows = 1
                delta         = 1/10
                ratio         = 3/4

--
-- hooks
--
myManageHook = manageDocks <+> manageHook defaultConfig 
