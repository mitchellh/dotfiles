import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar /home/mitchellh/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal = "urxvt"
    , modMask = mod4Mask
    , normalBorderColor = "#1F1F1F"
    , focusedBorderColor = "#2688DE"
    , workspaces = myWorkspaces
    , manageHook = manageHook defaultConfig <+> myManageHook
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")
    ]

myWorkspaces = ["1:web", "2:chat", "3:edit", "4", "5", "6"]

myLayout = avoidStruts $ layoutHints $
           onWorkspaces ["1:web", "2:chat", "3:edit"] Full $
           (Full ||| tiled (1/2))
  where
    tiled = Tall 1 (3/100)

myManageHook = composeAll . concat $
               [ [isDialog --> doFloat]
               , [isA x --> doShift "1:web" | x <- my1Shifts]
               , [isA x --> doShift "2:chat" | x <- my2Shifts]
               , [isA x --> doShift "3:edit" | x <- my3Shifts]
               ]
  where
    isA x = className =? x <||> title =? x <||> resource =? x <||> appName =? x
    my1Shifts = ["chromium-browser"]
    my2Shifts = ["irssi"]
    my3Shifts = ["emacs"]
