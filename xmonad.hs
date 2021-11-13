{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)

-- Imports for Polybar --
import qualified Codec.Binary.UTF8.String              as UTF8
import qualified DBus                                  as D
import qualified DBus.Client                           as D

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Operations (windows)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.SpawnOnce (spawnOnce, spawnOnOnce)
import XMonad.Util.Loggers
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.IfMax
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps

import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import XMonad.Actions.Search

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

main :: IO ()
main = 
  mkDbusClient >>= xmonad
    . withNavigation2DConfig def
    . ewmhFullscreen
    . ewmh
    . docks
    . myXConfig
  
myXConfig dbus = def
  { terminal           = "alacritty"
  , modMask            = mod4Mask
  , workspaces         = myWorkspaces
  
  , borderWidth        = 2
  , normalBorderColor  = "#928374"
  , focusedBorderColor = "#b16286"

  , keys               = \c -> mkKeymap c (myKeymap c)

  , startupHook        = myStartupHook
  , layoutHook         = myLayoutHook
  , manageHook         = manageHook def <+> myManageHook
  , logHook = myLogHook dbus
  }

myXPConfig = def
  { bgColor           = "#282828"
  , fgColor           = "#ebdbb2"
  , bgHLight          = "#ebdbb2"
  , fgHLight          = "#282828"
  , borderColor       = "#928374"
  , promptBorderWidth = 2
  , height            = 30
  , searchPredicate   = fuzzyMatch
  , sorter            = fuzzySort
  }

myPPConfig dbus = def
  { ppCurrent         = \w -> "%{o#fabd2f}%{+o}%{F#fabd2f}>" ++ w ++ ">%{F-}%{-o}"
  , ppHidden          = \w -> " %{F#b16286}" ++ w ++ "%{F-} "
  , ppHiddenNoWindows = \w -> " " ++ w ++ " "
  , ppSep             = ">>= "
  , ppWsSep           = "|"
  , ppTitle           = \t -> shorten 50 t
  , ppLayout          = const ""
  , ppOrder           = \[w, l, t] -> [w, t]
  , ppOutput          = dbusOutput dbus
  }

myWorkspaces = digitKeys

myKeymap = \c
  -> appKeys c
  ++ windowsFocus
  ++ windowsSwap
  ++ layoutKeys
  ++ workspaceKeys
  ++ screenshotKeys
  ++ promptKeys
  ++ popupKeys
  ++ audioKeys

windowsFocus =
  [ ("M1-<Tab>"     , windows W.focusDown)
  , ("M1-S-<Tab>"   , windows W.focusUp  )
  , ("M1-C-<Left>"  , windowGo L True    )
  , ("M1-C-<Down>"  , windowGo D True    )
  , ("M1-C-<Up>"    , windowGo U True    )
  , ("M1-C-<Right>" , windowGo R True    )
  ]

windowsSwap =
  [ ("M-<Tab>"      , windows W.swapDown)
  , ("M-S-<Tab>"    , windows W.swapUp  )
  , ("M1-S-<Left>"  , windowSwap L True )
  , ("M1-S-<Down>"  , windowSwap D True )
  , ("M1-S-<Up>"    , windowSwap U True )
  , ("M1-S-<Right>" , windowSwap R True )
  ]

layoutKeys =
  [ ("M-<Up>"    , withFocused (sendMessage . maximizeRestore))
  , ("M-<Down>"  , withFocused minimizeWindow                 )
  , ("M-S-<Down>", withLastMinimized maximizeWindowAndFocus   )
  , ("M-S-t"     , withFocused $ windows . W.sink             )
  , ("M-l"       , sendMessage NextLayout                     )
  ] ++
  [("M-C-" ++ k, sendMessage $ JumpToLayout l)
  | (k, l) <- zip digitKeys [ "Tall", "ThreeCol", "Grid", "Full"]
  ]

workspaceKeys =
  [ ("M-" ++ m ++ k, windows $ f i)
  | (i, k) <- zip myWorkspaces digitKeys
  , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
  ]

appKeys = \c ->
  [ ("M-S-c"       , kill                    ) 
  , ("M-S-<Return>", spawn $ terminal c      )
  , ("M-e"         , spawn "nautilus"        )
  , ("M-f"         , spawn "firefox"         )
  , ("M-w"         , spawn "codium"          )
  , ("M-t"         , spawn "telegram-desktop")
  , ("M-d"         , spawn "discord"         )
  ]  

screenshotKeys = 
  [ ("<Print>"    , spawn $ select     ++ toClip)
  , ("S-<Print>"  , spawn $ fullscreen ++ toClip)
  , ("C-<Print>"  , spawn $ active     ++ toClip)
 
  , ("M-<Print>"  , spawn $ select     ++ toFile)
  , ("M-S-<Print>", spawn $ fullscreen ++ toFile)
  , ("M-C-<Print>", spawn $ active     ++ toFile)
  ]
  where
    fullscreen = "maim -u"
    select = "maim -su"
    active = "maim -u -i $(xdotool getactivewindow)"
    toClip = " | xclip -selection clipboard -t image/png"
    toFile = " ~/Pictures/Screenshots/$(date +%Y-%m-%d_%H-%M-%S).png"

popupKeys =
  [ ("M-o"  , spawn launcher )
  , ("M-s"  , spawn leftPopup)
  , ("M-S-s", spawn ewwclose )  
  , ("M-v"  , spawn clipboard)
  , ("M-b"  , sendMessage ToggleStruts)
  ]
  where
    launcher  = "rofi -no-lazy-grab -show drun -modi run,drun,window -theme ~/.xmonad/rofi/style.rasi"
    ewwclose  = "exec ~/bin/eww close-all"
    leftPopup = "exec ~/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"    
    clipboard = "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}' -theme ~/.xmonad/rofi/style.rasi"

audioKeys =
  [ ("<XF86AudioPlay>", spawn "playerctl play-pause" )
  , ("<XF86AudioPrev>", spawn "playerctl previous"   )
  , ("<XF86AudioNext>", spawn "playerctl next"       )
  ]

promptKeys = 
  [ ("M-x"    , shellPrompt myXPConfig)
  , ("M-z"    , buildPrompt @RunScript)
  , ("M-S-x s", searchPrompt          )
  ]

myStartupHook = do
  spawnOnce "polybar -r -c ~/.xmonad/polybar/polybar top"
  spawnOnce "exec ~/bin/eww daemon"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "feh --bg-scale ~/.xmonad/bg-gruvbox.png"
  spawnOnce "picom"
  spawnOnce "greenclip daemon"
  spawnOnce "python ~/bin/jakowlew/getweather.py"
  spawnOnce "sleep 3 && setxkbmap -model pc105 -layout us,ru -option grp:win_space_toggle"
  spawnOnce "/etc/X11/xinit/xinitrc.d/50-systemd-user.sh ~/.xinitrc"
  spawnOnce "exec redshift -l 54.790311:32.050366"

myLayoutHook = avoidStruts
             $ smartBorders
             $ minimize
             $ maximizeWithPadding 0
             $ layout
  where
    layout = tiled
         ||| threeColMid
         ||| Grid
         ||| Full

    tiled = Tall nmaster delta ratio
    threeColMid = ThreeColMid nmaster delta ratio

    nmaster = 1
    ratio = 1 / 2
    delta = 5 / 100

myManageHook = composeAll
  [ isDialog  --> doFloat
  , isPolybar --> doLower
  ]
  where
    isPolybar = className =? "Polybar"

myLogHook dbus = dynamicLogWithPP $ myPPConfig dbus

moveTo i = doF $ W.shift (myWorkspaces !! i)

digitKeys :: [String]
digitKeys = map (:[]) ['1'..'9']

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = (D.signal opath iname mname)
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

class MyPrompt a where
  name    :: String
  compl   :: [String]
  handler :: String -> X ()

buildPrompt :: forall a. MyPrompt a => X ()
buildPrompt = inputPromptWithCompl myXPConfig (name @a) promptCompl ?+ (handler @a)
  where
    promptCompl = mkComplFunFromList' myXPConfig (compl @a)
    
searchPrompt = promptSearchBrowser myXPConfig "firefox" duckduckgo

data RunScript

instance MyPrompt RunScript where
  name  = "Script"
  compl = ["Test"]
  
  handler input = spawn script
    where
      script = M.findWithDefault "" input scriptMap
      scriptMap = M.fromList
        [ ("Test", "python ~/.xmonad/scripts/test.py")
        ]