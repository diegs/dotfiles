import System.IO
import System.Exit

import XMonad
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.CenteredMaster
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

myWorkspaces = map show [1..9]

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
  , ((mod4Mask, xK_i), spawn "gvim")
  , ((mod4Mask, xK_o), spawn "gnome-terminal")
  , ((mod4Mask, xK_p), spawn "dmenu_run -nf '#c0c5ce' -nb '#2b303b' -fn 'anonymous pro:pixelsize=12'")
  , ((mod4Mask, xK_n), spawn "touch ~/.pomodoro_session")
  , ((mod4Mask, xK_m), spawn "rm ~/.pomodoro_session")
  , ((mod4Mask, xK_Left), spawn "ncmpcpp prev")
  , ((mod4Mask, xK_Right), spawn "ncmpcpp next")
  , ((mod4Mask, xK_F8), spawn "ncmpcpp toggle")
  , ((mod4Mask, xK_F9), spawn "ncmpcpp volume -5")
  , ((mod4Mask, xK_F10), spawn "ncmpcpp volume +5")
  -- , ((mod4Mask, xK_d), promptWSGroupAdd myConfig "Name group: ")
  -- , ((mod4Mask, xK_g), promptWSGroupView myConfig "Go to group: ")
  -- , ((mod4Mask, xK_f), promptWSGroupForget myConfig "Forget group: ")
  ] ++ [
    -- swap screen order
    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2] , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "#c5c8c6" "" . shorten 100
  , ppCurrent = xmobarColor "#c5c8c6" "" . wrap "" ""
  , ppSep     = xmobarColor "#969896" "" " | "
  , ppUrgent  = xmobarColor "#cc6666" ""
                --  , ppLayout = const "" -- to disable the layout info on xmobar
  }

myStartupHook = do
  startupHook gnomeConfig
  -- addRawWSGroup "g1" [(S 0, "1"), (S 1, "2")]
  -- addRawWSGroup "g2" [(S 0, "3"), (S 1, "4")]
  -- addRawWSGroup "g3" [(S 0, "5"), (S 1, "6")]
  -- addRawWSGroup "g4" [(S 0, "7"), (S 1, "8")]
  setWMName "LG3D"
  -- spawn "unclutter -grab"
  spawn "setxkbmap -layout us -option ctrl:nocaps"
  spawn "gnome-settings-daemon"
  spawn "drive-sync"
  spawn "/usr/bin/xcompmgr -a"
  -- spawn "xset r rate 250 60"
  spawn "xsetroot -solid '#2b303b'"

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
  xmproc <- spawnPipe "MPD_PORT=6666 ~/.cabal/bin/xmobar ~/.xmobarrc"
  xmonad $ myConfig { logHook = myLogHook xmproc }
