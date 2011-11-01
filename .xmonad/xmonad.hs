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
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
-- Utils
import XMonad.Util.Font
import XMonad.Util.Run              (spawnPipe)
import XMonad.Util.Loggers


main = do 
        bar <- myBar
        xmonad $ defaultConfig
            { terminal = myTerminal 
            , borderWidth = myBorderWidth
            , normalBorderColor = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , workspaces = myWorkspaces
            , manageHook = myManageHook
            , layoutHook = myLayoutHook
            , logHook = myXmobarPP bar
            , modMask = myModMask
            }

--
-- xmonad 
--
myTerminal = "xterm"
myWorkspaces = ["1:web", "2:wiki", "3:dev"] ++ map show [4..9 :: Int]
myModMask = mod4Mask     -- Win or Cmd key 


myBorderWidth = 2
myNormalBorderColor = "#CCCCCC"
myFocusedBorderColor = "#FF0099"

--
-- UI
--
myDzenPP = dzenPP
myXmobarPP bar = dynamicLogWithPP xmobarPP
                    { ppCurrent = xmobarColor "yellow" "" . wrap "|" "|"
                    , ppHidden = xmobarColor "gray" ""
                    {-, ppHiddenNoWindows = -}
                    {-, ppUrgent = -}
                    , ppSep = " - " 
                    , ppWsSep = "·" 
                    , ppTitle = xmobarColor "black" "white" . shorten 50
                    , ppLayout = xmobarColor "green" "" . wrap "_" "_"
                    , ppOrder = \(ws:l:t:xs) -> [t,ws,l] ++ xs
                    , ppExtras = [battery, date "%H:%M · %a %d.%m.%Y"]
                    , ppOutput = hPutStrLn bar
                    }

myBar = spawnPipe "xmobar"

-- 
-- layouts
--
myLayoutHook = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| Full ||| Grid
            where
                tiled = Tall masterWindows delta ratio
                masterWindows = 1
                delta = 5/100
                ratio = 1/2

--
-- hooks
--
myManageHook = manageDocks <+> manageHook defaultConfig 
