import System.IO
import System.Exit
import qualified Data.Map as M

import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.CenteredMaster
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.Spiral
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W

myTerminal = "gnome-terminal"

myFocusFollowsMouse = False

myBorderWidth = 2

myModMask = mod4Mask

myNormalBorderColor = "#81a2be"

myFocusedBorderColor = "#cc6666"

--myWorkspaces = map show [1..9]

myWorkspaces = withScreens 2 $ map show [1..9]

myManageHook = fullFloatHook <+> manageHook gnomeConfig
  where fullFloatHook = composeAll [ isFullscreen --> doFullFloat ]

myLayout = avoidStruts $ tiled ||| reflectHoriz tiled ||| Mirror tiled ||| Grid ||| centerMaster Grid ||| spiral (1/2)
  where
     tiled = Tall nmaster delta ratio
     nmaster = 1
     ratio = 1/2
     delta = 3/100

myKeys =
  [ ((mod4Mask, xK_c), spawn "google-chrome")
  , ((mod4Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((mod4Mask, xK_x), spawn "gnome-screensaver-command -a")
  , ((mod4Mask, xK_o), spawn "gnome-terminal")
  , ((mod4Mask, xK_p), spawn "dmenu_run -nf '#c5c8c6' -nb '#1d1f21' -fn 'anonymous pro:pixelsize=12'")
  , ((mod4Mask, xK_n), spawn "touch ~/.pomodoro_session")
  , ((mod4Mask, xK_m), spawn "rm ~/.pomodoro_session")
--  , ((mod4Mask, xK_F8), spawn "xdotool key --clearmodifiers XF86AudioPrev")
--  , ((mod4Mask, xK_F9), spawn "xdotool key --clearmodifiers XF86AudioPlay")
--  , ((mod4Mask, xK_F10), spawn "xdotool key --clearmodifiers XF86AudioNext")
  ] ++ [
    -- workspaces are distinct by screen
    ((m .|. mod4Mask, k), windows $ onCurrentScreen f i) | (i, k) <- zip (workspaces' myConfig) [xK_1 .. xK_9] , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++ [
    -- swap screen order
    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2] , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

  {-
  ] ++
  [ ((mod4Mask .|. mask, key), f sc) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                                     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
  ] ++ [
    ((mod4Mask .|. m, k), windows $ f i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
                                         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
-}

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "#c5c8c6" "" . shorten 100
  , ppCurrent = xmobarColor "#969896" "" . wrap "" ""
  , ppSep     = xmobarColor "#969896" "" " | "
  , ppUrgent  = xmobarColor "#cc6666" ""
                --  , ppLayout = const "" -- to disable the layout info on xmobar
  }

myStartupHook = do
  startupHook gnomeConfig
  setWMName "LG3D"
  -- spawn "unclutter -grab"
  spawn "gnome-settings-daemon"
  spawn "drive-sync"
  spawn "/usr/bin/xcompmgr -a"
  -- spawn "xset r rate 250 60"
  spawn "xsetroot -solid '#1d1f21'"

myConfig = gnomeConfig
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , workspaces         = myWorkspaces
  , manageHook         = myManageHook
  , startupHook        = myStartupHook
  , layoutHook         = myLayout
                         --  , mouseBindings      = myMouseBindings
                         --  , handleEventHook    = myEventHook
  } `additionalKeys` myKeys

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
  xmonad $ myConfig { logHook = myLogHook xmproc }
