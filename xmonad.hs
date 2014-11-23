
-- needed software:
-- conky dzen2 xdotool  -- for status bar
-- feh                  -- for setting background
-- xrander              -- for multi-head
-- gmrun                -- for quick launch
-- amixer               -- for volume control(in alsa-utils)
-- scrot                -- for screenshot (gnome-screenshot -a can not show border clearly)
-- udiskie              -- for automount.

import XMonad
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Actions.DwmPromote   -- swap master like dwm
import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import System.IO                   -- hPutStrLn scope
import Control.Monad(liftM2)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Hooks.ManageHelpers(doFullFloat)
-- for volume control ...
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W   -- manageHook rules


main = do
  -- I use two dzen bars, the left one contains the Xomand workspaces
  -- and title etc., eth right one contains the output from conky
  -- with some stats etc.
  status <- spawnPipe myDzenStatus
  conky  <- spawnPipe myDzenConky

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    terminal      = "xfce4-terminal"
    , modMask     = myModMask
    , workspaces  = myWorkspaces
    , borderWidth = 3
    -- , normalBorderColor  = "#dddddd"
    -- , focusedBorderColor = "#0000ff"

    , layoutHook  = myLayout
    , logHook     = myLogHook status
    , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , startupHook = myStartupHook
    , focusFollowsMouse = False
    } `additionalKeys` myKeys

myModMask = mod4Mask

myWorkspaces =
  [ "1:-"
  , "2:www"
  , "3:mail"
  , "4:edit"
  , "5:file"
  , "6"
  , "7"
  , "8:media"
  , "9:chat"
  ]

myStartupHook = do
    -- set no beep
    spawn "xset -b"
    -- input method
    spawn "fcitx"
    -- modify by `xrandr -q`
    spawn "/usr/bin/xrandr --auto --output LVDS1 --primary --auto --output HDMI1 --right-of LVDS1 --auto --output VGA1 --right-of LVDS1"
    -- automount
    spawn "udiskie -2"
    -- background setting
    spawn "sleep 0.1; /usr/bin/feh --bg-scale ~/.xmonad/jzbq.jpeg"
    -- screensaver daemons
    spawn "cinnamon-screensaver"

myManageHook = composeAll . concat $
    [ [ className   =? c --> doF(W.shift "2:www") | c <- webApps]
    , [ className   =? "Thunderbird" --> doF(W.shift "3:mail")]
    , [ className   =? "Gvim" --> viewShift "4:edit"]
    , [ className   =? "Nemo" --> doF(W.shift "5:file")]
    , [ className   =? "Rhythmbox" --> doF(W.shift "8:media")]
    , [ className   =? "Mplayer" --> viewShift "8:media"]
    , [ className   =? c --> doF(W.shift "9:chat") | c <- ircApps]
    , [ resource    =? "TeamViewer.exe" <&&> title =? "Computers & Contacts" --> doIgnore]
    , [ resource    =? "TeamViewer.exe" --> viewShift "7"]
    ]
  where webApps       = ["Firefox"]
        ircApps       = ["Pidgin", "Virt-viewer"]
        viewShift     = doF . liftM2 (.) W.greedyView W.shift

myScreenshot = "scrot" ++ myScreenshotOptions
myScreenshotArea = "sleep 0.3s; scrot -s" ++ myScreenshotOptions
myScreenshotOptions = " -e 'mv $f ~/Pictures/' '%Y%m%dT%H%M%S_$wx$h_scrot.png'"

myKeys =
  -- run command
  [ ((mod1Mask, xK_F2), spawn "gmrun")
  , ((myModMask, xK_p), spawn "gmrun")
  -- classic alt-tab behaviour
  , ((mod1Mask, xK_Tab), cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab )
  -- lock screen
  , ((controlMask .|. mod1Mask, xK_l), spawn "cinnamon-screensaver-command --lock")
  -- print screen
  , ((controlMask, xK_Print), spawn myScreenshotArea)
  , ((0, xK_Print), spawn myScreenshot)
  -- reboot or shutdown
  -- TODO add comfirm box.
  , ((controlMask .|. shiftMask, xK_Delete), spawn "systemctl reboot")
  , ((controlMask .|. shiftMask, xK_Insert), spawn "systemctl poweroff")
  -- applications key map
  , ((myModMask .|. shiftMask, xK_w), spawn "firefox")
  , ((myModMask .|. shiftMask, xK_f), spawn "nemo --no-desktop")
  , ((myModMask .|. shiftMask, xK_m), spawn "thunderbird")
  , ((myModMask .|. shiftMask, xK_p), spawn "pidgin")
  , ((myModMask .|. shiftMask, xK_v), spawn "virt-viewer -c qemu:///system win7")
  -- volume control
  , ((0 , xF86XK_AudioLowerVolume), spawn "amixer set Master 4%-")
  , ((0 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 4%+")
  , ((0 , xF86XK_AudioMute), spawn "amixer set Master toggle")
  -- undo float windows.
  , ((myModMask, xK_t ), withFocused $ windows . W.sink)
  ]

-- the default layout is fullscreen with smartborders applied to all
myLayout = onWorkspace "8:media" fullL $ avoidStruts $ smartBorders ( full ||| mtiled ||| tiled )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    fullL   = noBorders $ Full

-- Statusbar
--
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -xs 1 -w 720 -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -xs 1 -x 720 -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -u -h '20' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=11'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " ""
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " ""
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " ""
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " ""
    , ppSep     = " "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)|" "|^ca()"
    , ppTitle   = dzenColor "#ffffff" ""
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 80 . dzenEscape
    }

