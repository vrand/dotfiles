-- Haskell libraries
import Data.Ratio                   ((%))
import System.IO

import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP, shorten, xmobarPP, xmobarColor)
import XMonad.Hooks.ManageDocks
-- Layout
import XMonad.Layout                
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
-- Utils
import XMonad.Util.Run              (spawnPipe)

 
main = do 
        xmproc <- spawnPipe "xmobar"
        xmonad $ defaultConfig
            { borderWidth = 2
            , normalBorderColor = "#CCCCCC"
            , focusedBorderColor = "#FF0099" 
            , workspaces = ["1:web", "2:wiki", "3:dev"] ++ map show [4..9 :: Int]
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
myLayoutHook = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| simpleTabbed ||| Full ||| Grid
            where
                tiled = Tall masterWindows delta ratio
                masterWindows = 1
                delta = 5/100
                ratio = 1/2

--
-- application specific settings
--
myManageHook = manageDocks <+> manageHook defaultConfig 
