{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, RankNTypes, FlexibleContexts #-}

module Main where

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Operations (windows)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.SpawnOnce (spawnOnce, spawnOnOnce)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

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
main = (xmonad
     . ewmhFullscreen
     . ewmh
     . withNavigation2DConfig def)
     =<< statusBar "xmobar" def toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConfig = def
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

myWorkspaces = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]

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
  , ("M-t"       , withFocused $ windows . W.sink             )
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
  [ ("M-S-c"       , kill              ) 
  , ("M-S-<Return>", spawn $ terminal c)
  , ("M-e"         , spawn "nautilus"  )
  , ("M-f"         , spawn "firefox"   )
  , ("M-w"         , spawn "codium"    )
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
  , ("M-b"  , spawn bartoggle)
  ]
  where
    launcher  = "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style"
    ewwclose  = "exec ~/bin/eww close-all"
    leftPopup = "exec ~/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"    
    clipboard = "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"
    bartoggle = "exec ~/bin/bartoggle"

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
  spawnOnce "exec ~/bin/bartoggle"
  spawnOnce "exec ~/bin/eww daemon"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "feh --bg-scale ~/.xmonad/bg-gruvbox.png"
  spawnOnce "picom"
  spawnOnce "greenclip daemon"
  spawnOnce "python ~/bin/jakowlew/getweather.py"
  spawnOnce "sleep 3 && setxkbmap -model pc105 -layout us,ru -option grp:win_space_toggle"
  spawnOnce "/etc/X11/xinit/xinitrc.d/50-systemd-user.sh ~/.xinitrc"
  spawnOnce "exec redshift -l 54.790311:32.050366"

myLayoutHook = smartBorders
             $ minimize
             $ maximizeWithPadding 0
             $ avoidStruts
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
  [ --isTelegram --> moveToThird
    isDialog   --> doFloat
  , className =? "Tint2" --> doIgnore
  ]
  where
    isTelegram = className =? "TelegramDesktop"

-- moveToThird = doF $ W.shift (myWorkspaces !! 2)

digitKeys :: [String]
digitKeys = map (:[]) ['1'..'9']

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