--Imports

--Base
import XMonad
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--Actions
import XMonad.Actions.Search
import XMonad.Actions.NoBorders

--Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName


--Layout
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances


--Util
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86


-- My preferred terminal program.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
--
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["dev", "www", "chat", "game", "music", "vbox", "misc"]
    where
        clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Border colors for unfocused and focused windows.
--
myNormalBorderColor  = "#e1d9c8"
myFocusedBorderColor = "#e1d9c8"

------------------------------------------------------------------------
-- Key bindings.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask,xK_Return ), spawn $ XMonad.terminal conf)
    , ((modMask,xK_space ), sendMessage NextLayout)
    , ((modMask,xK_n     ), refresh)
    , ((modMask,xK_Tab   ), windows W.focusDown)
    , ((modMask,xK_j     ), windows W.focusDown)
    , ((modMask,xK_k     ), windows W.focusUp  )
    , ((modMask,xK_h     ), sendMessage Shrink)
    , ((modMask,xK_l     ), sendMessage Expand)
    , ((modMask,xK_m     ), windows W.focusMaster  )
    , ((modMask,xK_f     ), sendMessage $ Toggle NBFULL)
    , ((modMask,xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask,xK_b     ), sendMessage ToggleStruts)
    , ((modMask .|. shiftMask, xK_q     ), kill)
    , ((modMask .|. shiftMask, xK_Return     ), spawn $ "firefox")
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask .|. shiftMask, xK_s     ), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modMask,  xK_g ),   withFocused toggleBorder)
    , ((modMask .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_r     ), spawn "xmonad --recompile && xmonad --restart")
    , ((mod1Mask, xK_F2), spawn $ "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
    , ((0, xK_F1), spawn $ "discord")
    , ((0, xK_F2), spawn $ "virtualbox")
--    , ((0, xK_F12), spawn $ "spotify")
    , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")
    , ((0, xK_Print), spawn $ "scrot 'Screenshot-%Y-%m-%d.jpg' -e 'mv $f ~/Pictures'")
    , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
--
-- The available layouts. Each layout is separated by |||.
--
myLayout = avoidStruts ( smartSpacingWithEdge 6 $ tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "heroic"         --> doFloat
    , className =? "Steam (Runtime)" --> doFloat
    , className =? "Wine"           --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling
--
myEventHook = mempty
------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
    setWMName "LG3D"
    spawnOnce "nitrogen --restore "
    spawnOnce "picom &"
    spawnOnce "xrandr --output eDP-1 --mode 1366x768 --pos 1920x0 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-2 --off"
    
------------------------------------------------------------------------
-- Run xmonad with the settings specified.
--
main = do
   xmproc <- spawnPipe "xmobar -x 0 /home/v0lix/.xmonad/xmobarrc"
   xmonad $ docks def {


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
        handleEventHook    = myEventHook <+> fullscreenEventHook,
        logHook            = dynamicLogWithPP xmobarPP
                               { ppOutput = hPutStrLn xmproc
                               , ppCurrent = xmobarColor "#e1d9c8" "" . wrap "[ " " ]" -- Current workspace in xmobar
                               , ppVisible = xmobarColor "#e1d9c8" ""                -- Visible but not current workspace
                               , ppHidden = xmobarColor "#e1d9c8" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                               , ppHiddenNoWindows = xmobarColor "#e1d9c8" ""        -- Hidden workspaces (no windows)
                               , ppTitle = xmobarColor "#e1d9c8" "" . shorten 60     -- Title of active window in xmobar
                               --, ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                               , ppUrgent = xmobarColor "#e1d9c8" "" . wrap "!" "!"  -- Urgent workspace
                               --, ppExtras  = [windowCount]                           -- # of windows current workspace
                               , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                               },
        startupHook        = myStartupHook
      }
