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
            { terminal = myTerminal 
            , borderWidth = myBorderWidth
            , normalBorderColor = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , workspaces = myWorkspaces
            , manageHook = myManageHook
            , layoutHook = myLayoutHook
            , logHook = dynamicLogWithPP $ myDzenBar bar
            , modMask = myModMask
            }

--
-- xmonad 
--
myTerminal = "xterm"
myWorkspaces = ["main", "web", "dev", "test", "social", "media", "extra"] 
myModMask = mod4Mask     -- Win or Cmd key 


--
-- aesthetics
--
myBorderWidth = 2
myNormalBorderColor = "#CCCCCC"
myFocusedBorderColor = "#FF0099"
myFont = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"

--
-- status bar
--
statusBarCmd = "dzen2 -bg '#1a1a1a' -fg '#777777' -h 16 -w 1280 -sa c -e '' -ta l -fn '-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1'" 
myBar = spawnPipe statusBarCmd

myDzenBar bar = defaultPP
                    { ppCurrent = dzenColor "yellow" "" . wrap "|" "|" 
                    , ppHidden = dzenColor "gray" "" 
                    , ppSep = " - " 
                    , ppWsSep = "·" 
                    , ppTitle = dzenColor "white" "" . wrap "<" ">" . shorten 50
                    , ppLayout = dzenColor "green" "" . wrap "_" "_"
                    , ppOrder = \(ws:l:t:xs) -> [ws,l,t] ++ xs
                    , ppExtras = [myBattery, myDate]
                    , ppOutput = hPutStrLn bar
                    }
                    where
                        myBattery = dzenColorL "red" "" battery
                        myDate = dzenColorL "orange" "" $ fixedWidthL AlignRight "" 20 $ date "%H:%M %a %d.%m.%Y" 


-- 
-- layouts
--
myLayoutHook = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| Full ||| Accordion
            where
                tiled = Tall masterWindows delta ratio
                masterWindows = 1
                delta = 5/100
                ratio = 1/2

--
-- hooks
--
myManageHook = manageDocks <+> manageHook defaultConfig 
