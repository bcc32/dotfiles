-- Warnings generated from compiling this module will be written to
-- ~/.xmonad/xmonad.errors.
{-# OPTIONS -Wall #-}

import XMonad
import XMonad.Hooks.DynamicLog    (xmobar)
import XMonad.Hooks.EwmhDesktops  (ewmh)
import XMonad.Layout.GridVariants (SplitGrid (..), Orientation (..))
import XMonad.Layout.Named        (named)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Layout.Tabbed       (tabbed, shrinkText)
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)
import XMonad.Prompt.Shell        (shellPrompt)
import XMonad.Util.EZConfig       (additionalKeysP)

myBindings :: [(String, X ())]
myBindings =
  [ ("M-d", changeDir def)
  , ("M-p", shellPrompt def)
  , ("M-x", spawn "gnome-screensaver-command -l")
  , ("M-S-p e", spawn "emacs")
  , ("M-S-p c", spawn "chromium || google-chrome")
  , ("M-S-p f", spawn "firefox")
  ]

main :: IO ()
main = do
  spawn "gnome-screensaver"
  cfg <- xmobar def
    { modMask           = mod1Mask
    , terminal          = "urxvtc"
    , layoutHook        = (workspaceDir "~" . smartBorders) myLayoutHook
    , focusFollowsMouse = False
    }
  xmonad $ ewmh $ cfg `additionalKeysP` myBindings
  where
    myLayoutHook
      =   (named "Split Grid T" . splitGrid) T
      ||| (named "Split Grid L" . splitGrid) L
      ||| (named "Tabbed" . tabbed shrinkText) def
      where
        masterRows  = 1
        slaveRows   = 1
        masterRatio = 5/8
        aspect      = realToFrac $ (1 + sqrt 5 :: Double) / 2
        increment   = 1/20
        splitGrid orientation =
          SplitGrid orientation masterRows slaveRows masterRatio aspect increment
