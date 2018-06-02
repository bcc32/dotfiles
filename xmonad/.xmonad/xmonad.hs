-- Warnings generated from compiling this module will be written to
-- ~/.xmonad/xmonad.errors.
{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (filterM)
import Data.Maybe (isJust)
import System.Directory (findExecutable)

import XMonad
import XMonad.Actions.SpawnOn       (manageSpawn, spawnHere)
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
import XMonad.Util.Run              (hPutStrLn, spawnPipe)

myBindings :: [(String, X ())]
myBindings =
  [ ("M-d", changeDir def)
  , ("M-p", shellPrompt def)
  , ("M-x", spawn "xscreensaver-command -l")
  , ("M-S-p e", spawnHere "emacs")
  , ("M-S-p v", spawnHere "gvim")
  , ("M-S-p c", spawnHere "chromium || google-chrome")
  , ("M-S-p f", spawnHere "firefox")
  ]

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

myXmobar :: LayoutClass l Window
         => String
         -> XConfig l
         -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar cmd = statusBar cmd xmobarPP toggleStrutsKey

myManageHook :: ManageHook
myManageHook = manageSpawn

-- List of terminal emulators, in order of preference.
myTerminals :: [String]
myTerminals =
  [ "alacritty"
  , "urxvtc"
  , "urxvt"
  , "xterm"
  ]

-- Most preferred installed terminal emulator.
myTerminal :: IO String
myTerminal = head <$> filterM isInstalled myTerminals
  where
    isInstalled = (isJust <$>) . findExecutable

main :: IO ()
main = do
  spawn "xscreensaver"

  -- favorite installed terminal emulator
  myTerminal' <- myTerminal

  -- Here, we start a pipe to the bottom xmobar and write exactly one empty
  -- line to it.  We treat the bottom xmobar differently because:
  --
  -- 1. The default statusBar implementation writes the current layout and
  -- window title to the pipe; if the bottom xmobar doesn't read from the pipe,
  -- the pipe will fill up and xmonad will become stuck trying to write to it.
  --
  -- 2. We don't want the bottom xmobar to actually display anything, but no
  -- built-in plugins for xmobar read stdin and discard it; since compiling
  -- xmobar from source to add another plugin seems like a right pain, we
  -- simply use StdinReader but don't write anything other than a single empty
  -- line.
  --
  -- 3. If we didn't open a pipe at all, and simply `spawn`ed the bottom
  -- xmobar, reloading the xmonad config with MOD-q will not kill it, and
  -- instead a second one will be spawned on top of the existing xmobar, which
  -- is rather inconvenient.
  h <- spawnPipe "xmobar ~/.xmonad/xmobarrc-bottom"

  return $ def
    { modMask           = mod1Mask
    , manageHook        = myManageHook <+> manageHook def
    , terminal          = myTerminal'
    , layoutHook        = (workspaceDir "~" . smartBorders) myLayoutHook
    , focusFollowsMouse = False
    , startupHook = liftIO $ hPutStrLn h ""
    } `additionalKeysP` myBindings
  >>= myXmobar "xmobar ~/.xmonad/xmobarrc-top"
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
