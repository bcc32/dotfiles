-- Warnings generated from compiling this module will be written to
-- ~/.xmonad/xmonad.errors.
{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Hooks.DynamicLog      (statusBar, xmobarPP)
import XMonad.Hooks.EwmhDesktops    (ewmh)
import XMonad.Hooks.ManageDocks     (AvoidStruts)
import XMonad.Layout.GridVariants   (SplitGrid (..), Orientation (..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Named          (named)
import XMonad.Layout.NoBorders      (smartBorders)
import XMonad.Layout.Tabbed         (tabbed, shrinkText)
import XMonad.Layout.WorkspaceDir   (changeDir, workspaceDir)
import XMonad.Prompt.Shell          (shellPrompt)
import XMonad.Util.EZConfig         (additionalKeysP)

myBindings :: [(String, X ())]
myBindings =
  [ ("M-d", changeDir def)
  , ("M-p", shellPrompt def)
  , ("M-x", spawn "gnome-screensaver-command -l")
  , ("M-S-p e", spawn "emacs")
  , ("M-S-p c", spawn "chromium || google-chrome")
  , ("M-S-p f", spawn "firefox")
  ]

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

-- FIXME doesn't quote rcFile, since that stops the interpolation of "~" in
-- paths.
myXmobar :: LayoutClass l Window
         => String
         -> XConfig l
         -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar rcFile = statusBar ("xmobar " ++ rcFile) xmobarPP toggleStrutsKey

-- TODO investigate MOD-, and MOD-. bindings not functioning
main :: IO ()
main = do
  spawn "gnome-screensaver"
  return $ def
    { modMask           = mod1Mask
    , terminal          = "urxvtc"
    , layoutHook        = (workspaceDir "~" . smartBorders) myLayoutHook
    , focusFollowsMouse = False
    } `additionalKeysP` myBindings
  -- FIXME kill both top and bottom xmobar when restarting xmonad
  >>= myXmobar "~/.xmonad/xmobarrc-top"
  >>= myXmobar "~/.xmonad/xmobarrc-bottom"
  >>= xmonad . ewmh
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
