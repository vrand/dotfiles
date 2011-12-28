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
import XMonad.Layout.ToggleLayouts
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
-- StackSet
import qualified XMonad.StackSet as W
-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.CustomKeys
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

-- TODO
--  explicit imports

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
myWorkspaces = ["dev", "test", "www", "media", "sys"]
myModMask    = mod4Mask     -- Win or Cmd key


--
-- aesthetics
--
myBorderWidth        = 2
myNormalBorderColor  = "#CCCCCC"
myFocusedBorderColor = "#FF0099"
myFont               = "-xos4-terminus-bold-r-normal--22-220-72-72-c-110-iso8859-1"

-- 
-- misc
--
myNotes = "/home/dialelo/vimwiki/process.wiki"

--
-- status bar
--
myBar = spawnPipe "xmobar"

-- there is more customization to xmobar in the .xmobarrc file
myXmobar bar = defaultPP
                { ppCurrent = xmobarColor "cyan" "" . wrap "[" "]"
                , ppHidden  = xmobarColor "white" ""
                , ppSort    = fmap (.scratchpadFilterOutWorkspace) getSortByTag
                , ppSep     = " - "
                , ppWsSep   = "·"
                --, ppTitle   = xmobarColor "#FF6666" "" . shorten 60
                , ppLayout  = xmobarColor "green" ""
                , ppOrder   = \(ws:l:_) -> [ws,l]
                , ppOutput  = hPutStrLn bar
                }


-- 
-- layouts
--
myLayoutHook = myTiledLayout       |||
               myMirrorTiledLayout |||
               myFullScreenLayout  

myTiledLayout = renamed [Replace "tiled"] $ spacing 3 . avoidStruts . smartBorders $ tiled
                where
                    tiled         = Tall masterWindows delta ratio
                    masterWindows = 1
                    delta         = 1/10
                    ratio         = 3/4

myMirrorTiledLayout = renamed [Replace "mirror"] $ spacing 3 . avoidStruts . smartBorders $ Mirror $ Tall 1 (1/10) (3/4) -- TODO

myFullScreenLayout  = renamed [Replace "full"] $ noBorders Full


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
            [ ((myModMask,               xK_f)        , spawn "firefox")                          
             -- toggle between last two workspaces
            , ((myModMask,               xK_a)        , toggleWS)                                 
             -- open the scratchpad
            , ((myModMask,               xK_s)        , scratchpad)                 
             -- prompt for a workspace's name and move to it
            , ((myModMask .|. shiftMask, xK_s)        , selectWorkspace myPrompt)                 
             -- rename the current workspace
            , ((myModMask,               xK_comma)    , renameWorkspace myPrompt)                 
             -- add a note to process it later
            , ((myModMask,               xK_g)        , appendFilePrompt myPrompt myNotes)
             -- quickly search for man pages 
            , ((myModMask,               xK_i)        , manPrompt myPrompt)
             -- prompt for XMonad actions 
            , ((myModMask,               xK_x)        , xmonadPrompt myPrompt)
             -- run program or navigate to it if it's already running
            , ((myModMask,               xK_p)        , runOrRaisePrompt myPrompt)
             -- ssh prompt
            , ((myModMask,               xK_c)        , sshPrompt myPrompt)
             -- add a workspace
            , ((myModMask,               xK_n)        , withWorkspace myPrompt addHiddenWorkspace)
             -- remove current workspace (potentially dangerous)
            , ((myModMask .|. shiftMask, xK_BackSpace), removeWorkspace)   
             -- TODO avoid redundancy
             -- navigation between workspaces
            , ((myModMask              , xK_d)        , windows $ W.greedyView "dev")
            , ((myModMask              , xK_t)        , windows $ W.greedyView "test")
            , ((myModMask              , xK_w)        , windows $ W.greedyView "www")
            , ((myModMask              , xK_m)        , windows $ W.greedyView "media")
            , ((myModMask              , xK_y)        , windows $ W.greedyView "sys")
             -- shift windows to workspaces
            , ((myModMask .|. shiftMask, xK_d)        , windows $ W.greedyView "dev")
            , ((myModMask .|. shiftMask, xK_t)        , windows $ W.greedyView "test")
            , ((myModMask .|. shiftMask, xK_w)        , windows $ W.greedyView "www")
            , ((myModMask .|. shiftMask, xK_m)        , windows $ W.greedyView "media")
            , ((myModMask .|. shiftMask, xK_y)        , windows $ W.greedyView "sys")
            -- volume control
            -- TODO
            -- bindings for switching between layouts
            --  TODO full
            --  TODO tiled
            --  TODO mirror
            ] 
            where
                scratchpad = scratchpadSpawnActionTerminal myTerminal

myKeys = customKeys keysToDel keysToAdd


--
-- hooks
--
myManageHook = composeAll
            [ className =? "Firefox" --> doShift "www"
            , className =? "luakit"  --> doShift "www"
            , className =? "Spotify" --> doShift "media"
            --, className =? "dia"     --> doFloat
            , className =? "trayer"  --> doIgnore
            , manageDocks
            ] <+> manageScratchpad

manageScratchpad = scratchpadManageHook (W.RationalRect left top width height)
            where
                height = 0.4            -- 40%
                width  = 1              -- 100%
                top    = 0              -- distance from top
                left   = 0              -- distance from left
