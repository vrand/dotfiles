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

dzenFont = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"
dzenForegroundColor = "#777777"
dzenBackgroundColor = "#090909"

--
-- status bar
--
statusBarCmd = "dzen2 " ++
               "-bg '" ++ dzenBackgroundColor ++ "' " ++ 
               "-fg '" ++ dzenForegroundColor ++ "' " ++
               "-h 16 -w 1280 -sa c -ta l" ++
               " -fn '" ++ dzenFont ++ "'"
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
                delta = 1/10
                ratio = 3/4

--
-- hooks
--
myManageHook = manageDocks <+> manageHook defaultConfig 
