-- XMonad
import XMonad
-- Actions
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- Layout
import XMonad.Layout                
import XMonad.Layout.NoBorders     
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
-- StackSet
import qualified XMonad.StackSet as W
-- Utils
import XMonad.Util.Run
import XMonad.Util.CustomKeys
import XMonad.Util.Loggers

-- TODO
--  scratchpad
--  key bindings for navigating to the workspaces
--

main = do 
        bar <- myBar
        xmonad $ defaultConfig
            { terminal           = myTerminal
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , workspaces         = myWorkspaces
            , manageHook         = myManageHook
            , keys               = myKeys
            , layoutHook         = myLayoutHook
            , logHook            = dynamicLogWithPP $ myXmobar bar
            , modMask            = myModMask
            } 
              

--
-- xmonad 
--
myTerminal   = "urxvt"
myWorkspaces = ["console", "dev", "www", "nav", "media", "sys"]
myModMask    = mod4Mask     -- Win or Cmd key


--
-- aesthetics
--
myBorderWidth        = 2
myNormalBorderColor  = "#CCCCCC"
myFocusedBorderColor = "#FF0099"
myFont               = "-xos4-terminus-bold-r-normal--22-220-72-72-c-110-iso8859-1"

--
-- status bar
--
myBar = spawnPipe "xmobar"

-- there is more customization to xmobar in the .xmobarrc file
myXmobar bar = defaultPP
                { ppCurrent = xmobarColor "cyan" ""
                , ppHidden  = xmobarColor "white" ""
                , ppSep     = " - "
                , ppWsSep   = "·"
                , ppTitle   = xmobarColor "#FF6666" "" . shorten 60
                , ppLayout  = xmobarColor "green" ""
                , ppOrder   = \(ws:l:t:xs) -> [ws,l,t] ++ xs
                , ppOutput  = hPutStrLn bar
                }


-- 
-- layouts
--
myLayoutHook = myTiledLayout       |||
               myMirrorTiledLayout |||
               myFullScreenLayout

myTiledLayout = renamed [Replace "t"] $ spacing 3 . avoidStruts . smartBorders $ tiled
                where
                    tiled         = Tall masterWindows delta ratio
                    masterWindows = 1
                    delta         = 1/10
                    ratio         = 3/4

myMirrorTiledLayout = renamed [Replace "m"] $ spacing 3 . avoidStruts . smartBorders $ Mirror $ Tall 1 (1/10) (3/4) -- TODO

myFullScreenLayout  = renamed [Replace "f"] $ noBorders Full

--
-- prompt
--
myPromptBgColor       = "#CCCCCC"
myPromptFgColor       = "#000000"
myPromptBgHLightColor = "#000000"
myPromptFgHLightColor = "#CC3333"
myPromptBorderWidth   = 0
myPromptHistorySize   = 10000
myPromptHeight        = 25

myPrompt :: XPConfig
myPrompt = defaultXPConfig
            { position          = Top
            , font              = myFont
            , bgColor           = myPromptBgColor
            , fgColor           = myPromptFgColor
            , fgHLight          = myPromptFgHLightColor
            , bgHLight          = myPromptBgHLightColor
            , promptBorderWidth = myPromptBorderWidth
            , height            = myPromptHeight
            }

--
-- keys
--
keysToDel :: XConfig l -> [(KeyMask, KeySym)] 
keysToDel XConfig {modMask = modm} = 
            [ ((mod4Mask,                xK_p))
            ]

keysToAdd :: XConfig l -> [((KeyMask, KeySym), X ())]
keysToAdd conf@(XConfig {modMask = modm}) = 
            [ ((mod4Mask,               xK_f)        , spawn "firefox")                          
             -- toggle between last two workspaces
            , ((mod4Mask,               xK_a)        , toggleWS)                                 
             -- prompt for a workspace's name and move to it
            , ((mod4Mask,               xK_s)        , selectWorkspace myPrompt)                 
             -- rename the current workspace
            , ((mod4Mask,               xK_comma)    , renameWorkspace myPrompt)                 
             -- add a workspace
            , ((mod4Mask,               xK_n)        , withWorkspace myPrompt addHiddenWorkspace)
             -- remove current workspace (potentially dangerous)
            , ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace)   
             -- quickly search for man pages 
            , ((mod4Mask,               xK_m)        , manPrompt myPrompt)
             -- prompt for XMonad actions 
            , ((mod4Mask,               xK_x)        , xmonadPrompt myPrompt)
             -- run program or navigate to it if it's already running
            , ((mod4Mask,               xK_p)        , runOrRaisePrompt myPrompt)
             -- ssh prompt
            , ((mod4Mask,               xK_c)        , sshPrompt myPrompt)
             -- navigation between workspaces
            , ((mod4Mask .|. shiftMask, xK_s),       , windows $ W.greedyView "sys")
            ] 

myKeys    = customKeys keysToDel keysToAdd


--
-- hooks
--
myManageHook = composeAll
            [ className =? "Firefox" --> doShift "www"
            , className =? "luakit"  --> doShift "www"
            , className =? "Spotify" --> doShift "media"
            , className =? "dia"     --> doFloat
            , className =? "trayer"  --> doIgnore
            , manageDocks
            ]
