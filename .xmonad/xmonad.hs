import Data.Ratio                   ((%))
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP, shorten, xmobarPP, xmobarColor)
import XMonad.Hooks.ManageDocks
import XMonad.Layout                
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Grid
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
                tiled = Tall masterWindows ratio delta
                masterWindows = 1
                -- golden ratio
                ratio = (1 + toRational (sqrt 5.0))/2
                -- percent of screen incremented when resizing panes
                delta = 10%100

--
-- application specific settings
--
myManageHook = manageDocks <+> manageHook defaultConfig 
