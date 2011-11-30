-- Haskell libraries
import Data.Ratio
import System.IO
-- XMonad
import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- Layout
import XMonad.Layout                
import XMonad.Layout.NoBorders     
import XMonad.Layout.Tabbed
-- Utils
import XMonad.Util.Run             
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
myWorkspaces = ["dev", "test", "web", "mail", "media", "system", "extra"]
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

-- ther is more customization to xmobar in the .xmobarrc file
myXmobar bar = defaultPP
                { ppCurrent = xmobarColor "gray" "blue" . wrap "|" "|"
                , ppHidden  = xmobarColor "gray" ""
                , ppSep     = " - "
                , ppWsSep   = "·"
                , ppTitle   = xmobarColor "white" "" . wrap "<" ">" . shorten 60
                , ppLayout  = xmobarColor "green" "" . wrap "_" "_"
                , ppOrder   = \(ws:l:t:xs) -> [ws,l,t] ++ xs
                , ppOutput  = hPutStrLn bar
                }


-- 
-- layouts
--
myLayoutHook = smartBorders . avoidStruts $ tiled        |||
                                            Mirror tiled |||
                                            Full         
                where
                    tiled         = Tall masterWindows delta ratio
                    masterWindows = 1
                    delta         = 1/10
                    ratio         = 3/4

--
-- hooks
--
myManageHook = manageDocks <+> manageHook defaultConfig 
