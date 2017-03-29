import System.IO
import System.Exit

import XMonad
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen as X.L.F
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.ThreeColumns
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W

myTerminal = "urxvt"

myFocusFollowsMouse = False

myBorderWidth = 2

myModMask = mod4Mask

myNormalBorderColor = "#81a2be"

myFocusedBorderColor = "#cc6666"

myExtraWorkspaces = [(xK_0, "0"),(xK_minus, "-"),(xK_quoteleft, "=")]

myWorkspaces = map show [1..9] ++ (map snd myExtraWorkspaces)

myManageHook = manageHook gnomeConfig

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| ThreeColMid 1 (3/100) (1/2)
  where
     tiled = Tall nmaster delta ratio
     nmaster = 1
     ratio = 1/2
     delta = 3/100

myKeys =
  [ ((mod4Mask, xK_c), spawn "google-chrome")
  , ((mod4Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((mod4Mask, xK_x), spawn "gnome-screensaver-command -a")
  , ((mod4Mask, xK_o), spawn myTerminal)
  , ((mod4Mask, xK_p), spawn "dmenu_run -nf '#c0c5ce' -nb '#1d1f21' -fn 'envypn-15'")
  , ((mod4Mask, xK_n), spawn "touch ~/.pomodoro_session")
  , ((mod4Mask, xK_m), spawn "rm ~/.pomodoro_session")
  , ((mod4Mask, xK_s), sendMessage ToggleStruts)
  , ((mod4Mask, xK_u), sendMessage Shrink)
  , ((mod4Mask, xK_i), sendMessage Expand)
  , ((mod4Mask, xK_h), windowGo L False)
  , ((mod4Mask, xK_k), windowGo U False)
  , ((mod4Mask, xK_j), windowGo D False)
  , ((mod4Mask, xK_l), windowGo R False)
  , ((mod4Mask .|. shiftMask, xK_h), windowSwap L False)
  , ((mod4Mask .|. shiftMask, xK_k), windowSwap U False)
  , ((mod4Mask .|. shiftMask, xK_j), windowSwap D False)
  , ((mod4Mask .|. shiftMask, xK_l), windowSwap R False)
  ] ++ [
    ((myModMask, key), (windows $ W.greedyView ws)) | (key,ws) <- myExtraWorkspaces
  ] ++ [
    ((myModMask .|. shiftMask, key), (windows $ W.shift ws)) | (key,ws) <- myExtraWorkspaces
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
  setWMName "LG3D"

myEventHook = handleEventHook gnomeConfig

myConfig xmproc = ewmh gnomeConfig
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
  , handleEventHook    = myEventHook
  , logHook            = myLogHook xmproc
  } `additionalKeys` myKeys

main = do
  xmproc <- spawnPipe "MPD_PORT=6666 xmobar ~/.xmobarrc"
  let config = myConfig xmproc
   in xmonad $ fullscreenSupport $ config --withNavigation2DConfig config $ config
