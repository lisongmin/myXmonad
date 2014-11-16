
-- needed software:
-- conky dzen2 xdotool  -- for status bar
-- feh                  -- for setting background
-- xrander              -- for multi-head
-- gmrun                -- for quick launch
-- amixer               -- for volume control
-- scrot                -- for screenshot (gnome-screenshot -a can not show border clearly)

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
    terminal      = "gnome-terminal --hide-menubar"
    , modMask     = myModMask
    , workspaces  = myWorkspaces
    , borderWidth = 3
    -- , normalBorderColor  = "#dddddd"
    -- , focusedBorderColor = "#0000ff"

    , layoutHook  = myLayout
    , logHook     = myLogHook status
    , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , startupHook = myStartupHook
    } `additionalKeys` myKeys

myModMask = mod1Mask

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
    spawn "/usr/bin/xrandr --auto --output eDP1 --primary --auto --output HDMI1 --right-of eDP1 --auto --output VGA1 --right-of eDP1"
    -- background setting
    spawn "sleep 0.1; /usr/bin/feh --bg-scale ~/.xmonad/jzbq.jpeg"

myManageHook = composeAll . concat $
    [ [ className   =? c --> doF(W.shift "2:www") | c <- webApps]
    , [ className   =? "Thunderbird" --> doF(W.shift "3:mail")]
    , [ className   =? "Gvimdiff" --> doF(W.shift "4:edit")]
    , [ className   =? "Gvim" --> doF(W.shift "4:edit")]
    , [ className   =? "Nemo" --> doF(W.shift "5:file")]
    , [ className   =? "Rhythmbox" --> doF(W.shift "8:media")]
    , [ className   =? "Mplayer" --> doF(W.shift "8:media")]
    , [ className   =? c --> doF(W.shift "9:chat") | c <- ircApps]
    , [ resource   =? "TeamViewer.exe" --> doF(W.shift "7")]
    ]
  where webApps       = ["Firefox"]
        ircApps       = ["Pidgin", "Virt-viewer"]

myScreenshot = "scrot" ++ myScreenshotOptions
myScreenshotArea = "sleep 0.3s; scrot -s" ++ myScreenshotOptions
myScreenshotOptions = " -e 'mv $f ~/Pictures/' '%Y%m%dT%H%M%S_$wx$h_scrot.png'"

myKeys =
  -- run command
  [ ((myModMask, xK_F2), spawn "gmrun")
  , ((myModMask, xK_p), spawn "gmrun")
  -- , ((mod1Mask, xK_Tab), cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
  -- lock screen
  , ((controlMask .|. mod1Mask, xK_l), spawn "gnome-screensaver-command --lock")
  -- print screen
  , ((controlMask, xK_Print), spawn myScreenshotArea)
  , ((0, xK_Print), spawn myScreenshot)
  -- reboot or shutdown
  , ((controlMask .|. shiftMask, xK_Delete), spawn "systemctl reboot")
  , ((controlMask .|. shiftMask, xK_Insert), spawn "systemctl poweroff")
  -- applications key map
  , ((mod4Mask, xK_w), spawn "firefox")
  , ((mod4Mask, xK_f), spawn "nemo --no-desktop")
  , ((mod4Mask, xK_m), spawn "thunderbird")
  , ((mod4Mask, xK_p), spawn "pidgin")
  , ((mod4Mask, xK_v), spawn "virt-viewer -c qemu:///system win7")
  -- volume control
  , ((0 , xF86XK_AudioLowerVolume), spawn "amixer set Master 4%-")
  , ((0 , xF86XK_AudioRaiseVolume), spawn "amixer set Master 4%+")
  , ((0 , xF86XK_AudioMute), spawn "amixer set Master toggle")
  ]

-- the default layout is fullscreen with smartborders applied to all
myLayout = avoidStruts $ smartBorders ( full ||| mtiled ||| tiled )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

    -- Teamviewer windows:
    -- File transfer
    -- TeamViewer Pannel
    -- - TeamViewer -
    -- TeamViewer
    -- Computers & Contacts
    --
-- Statusbar
--
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -xs 1 -w 1024 -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -xs 1 -x 1024 -ta 'r'" ++ myDzenStyle
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

