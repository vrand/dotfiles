import System.IO (hPutStrLn)
import System.Exit

import Data.Monoid
import qualified Data.Map as M

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import qualified XMonad.StackSet as W

myTerminal = "urxvt"
myLauncher = "/home/dialelo/bin/launcher"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask = mod4Mask

myWorkspaces = ["org", "dev", "social", "www", "media", "sys", "etc"]
myWorkspacesKeys = [xK_o, xK_d, xK_i, xK_w, xK_m, xK_y, xK_e]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#cb4030"

myScratchPad = myTerminal -- ++ " -e mux scratchpad"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launcher
    , ((modm,               xK_r     ), spawn myLauncher)

    -- scratchpad
    , ((modm,               xK_s     ), scratchpadSpawnActionTerminal myScratchPad)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    {-, ((modm,               xK_Return), windows W.swapMaster)-}

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspacesKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    {-++-}
    {-[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))-}
        {-| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]-}
        {-, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]-}


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = spacing 3 $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ] <+> manageScratchpad

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.4    -- height
        w = 1      -- width
        t = 0.2    -- top offset
        l = 0      -- left offset

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Forward the window information to the left dzen bar and format it
noScratchpad :: String -> String
noScratchpad ws = if ws == "NSP" then "" else ws

myLeftBarColor = "#FFFFFF"
myLeftBarBackgroundColor = "#000000"

myCurrentColor = "#FFA826"
myCurrentBackgroundColor = myLeftBarBackgroundColor
myPPCurrent = dzenColor myCurrentColor myCurrentBackgroundColor

myLeftBarHiddenColor = "#EEEEEE"
myLeftBarHiddenBackgroundColor = myLeftBarBackgroundColor
myPPHidden s = dzenColor myLeftBarHiddenColor myLeftBarHiddenBackgroundColor $ noScratchpad s

myLeftBarHiddenNoWindowsColor = "#AAAAAA"
myPPHiddenNoWindows s = dzenColor myLeftBarHiddenNoWindowsColor myLeftBarHiddenBackgroundColor $ noScratchpad s

myLeftBarWsColor = myLeftBarColor
myLeftBarWsBackgroundColor = myLeftBarBackgroundColor

myLeftBarUrgentColor = myLeftBarColor
myLeftBarUrgentBackgroundColor = "#FF2626"
myPPUrgent = dzenColor myLeftBarUrgentColor myLeftBarUrgentBackgroundColor

myLeftBarTitleColor = "#CCB90A"
myLeftBarTitleBackgroundColor = myLeftBarBackgroundColor
myPPTitle = dzenColor myLeftBarTitleColor myLeftBarTitleBackgroundColor

myWsSeparatorColor = "#00FFB3"
myWsSeparatorBackgroundColor = myLeftBarBackgroundColor
myPPWsSep = dzenColor myWsSeparatorColor myWsSeparatorBackgroundColor sep
    where
        spacing = "  "
        symbol = "·"
        sep = wrap spacing spacing symbol

mySeparatorColor = "#33FFC2"
mySeparatorBackgroundColor = myLeftBarBackgroundColor
myPPSep = dzenColor mySeparatorColor mySeparatorBackgroundColor sep
    where
        spacing = "      "
        symbol = "|"
        sep = wrap spacing spacing symbol

myLogHook h = dynamicLogWithPP $ myDzenPP
    { ppCurrent = myPPCurrent
    , ppTitle = myPPTitle
    , ppHidden = myPPHidden
    , ppHiddenNoWindows = myPPHiddenNoWindows
    , ppSep = myPPSep
    , ppWsSep = myPPWsSep
    , ppUrgent = myPPUrgent
    , ppOutput = hPutStrLn h
    }


-- dzen styles
surroundWithTicks :: String -> String
surroundWithTicks = wrap "'" "'"

myDzen = "dzen2"
myDzenYOffset = "0"
myDzenHeight = "24"

myDzenStyle  = " -h " ++ surroundWithTicks myDzenHeight ++
               " -y " ++ surroundWithTicks myDzenYOffset

-- Left bar contains XMonad information
myLeftBarWidth = "800"
myLeftBarXOffset = "0"
myLeftBarAlignment = "l"

myLeftBar = myDzen ++
            " -x " ++ surroundWithTicks myLeftBarXOffset ++
            " -w " ++ surroundWithTicks myLeftBarWidth ++
            " -ta " ++ surroundWithTicks myLeftBarAlignment ++ myDzenStyle

-- Right bar contains system stats
myRightBarWidth = "400"
myRightBarXOffset = myLeftBarWidth
myRightBarAlignment = "r"

myRightBar = "conky -c ~/.conkyrc | " ++
                myDzen ++
                " -x " ++ surroundWithTicks myRightBarXOffset ++
                " -w " ++ surroundWithTicks myRightBarWidth ++
                " -ta " ++ surroundWithTicks myRightBarAlignment ++ myDzenStyle

-- Very plain formatting, non-empty workspaces are highlighted,
-- urgent workspaces (e.g. active IM window) are highlighted in red
myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#ff9326" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = "  "
    , ppLayout  = \y -> map head $ words y
    , ppTitle   = dzenColor "#ffffff" "" . wrap " " " "
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
        status <- spawnPipe myLeftBar
        conky <- spawnPipe myRightBar

        xmonad =<< xmobar defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook status,
        startupHook        = myStartupHook
    }
