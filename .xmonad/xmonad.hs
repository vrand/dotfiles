import Data.Ratio                   ((%))
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP, shorten, xmobarPP, xmobarColor)
import XMonad.Hooks.ManageDocks
import XMonad.Layout                
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Util.Run              (spawnPipe)

 
main = do 
        xmproc <- spawnPipe "xmobar"
        xmonad $ defaultConfig
            { borderWidth = 2
            , normalBorderColor = "#CCCCCC"
            , focusedBorderColor = "#FF0099" 
            , workspaces = ["1:wiki", "2:web"] ++ map show [3..9 :: Int]
            , manageHook = myManageHook
            , layoutHook = myLayoutHook
            , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "white" "blue" . shorten 50
                }
            -- Win or Cmd key as mod key
            , modMask = mod4Mask
            }

-- 
-- layouts
--
myLayoutHook = smartBorders $ avoidStruts tiled ||| avoidStruts (Mirror tiled) ||| Full ||| avoidStruts Grid
            where
                tiled = ResizableTall masterWindows delta ratio [] 
                masterWindows = 1
                delta = 5/100
                ratio = 1/2

--
-- application specific settings
--
myManageHook = manageDocks <+> manageHook defaultConfig 
