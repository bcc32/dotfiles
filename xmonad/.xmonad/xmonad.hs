-- Warnings generated from compiling this module will be written to
-- ~/.xmonad/xmonad.errors.
{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe (fromJust, isJust)
import System.Directory (findExecutable)

import XMonad
import XMonad.Actions.SpawnOn       (manageSpawn, spawnHere)
import XMonad.Hooks.DynamicLog      (statusBar, xmobarPP)
import XMonad.Hooks.EwmhDesktops    (ewmh)
import XMonad.Hooks.ManageDocks     (AvoidStruts)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Named          (named)
import XMonad.Layout.NoBorders      (smartBorders)
import XMonad.Layout.Tabbed         (tabbed, shrinkText)
import XMonad.Layout.WorkspaceDir   (changeDir, workspaceDir)
import XMonad.Prompt.Shell          (shellPrompt)
import XMonad.Util.EZConfig         (additionalKeysP)
import XMonad.Util.Run              (hPutStrLn, spawnPipe)

import XMonad.Layout.BinarySpacePartition
  ( emptyBSP
  , ResizeDirectional(..)
  , Direction2D(..)
  , Rotate(..)
  , Swap(..)
  )

myBindings :: IO [(String, X ())]
myBindings = do
  myChrome <- firstInstalled ["chromium", "google-chrome"]
  return
    [ ("M-d", changeDir def)
    , ("M-p", shellPrompt def)
    , ("M-x", spawn "xscreensaver-command -l")
    , ("M-S-p e", spawnHere "emacsclient -ca=''")
    , ("M-S-p v", spawnHere "gvim")
    , ("M-S-p c", spawnHere myChrome)
    , ("M-S-p f", spawnHere "firefox")
    , ("M-M1-l", sendMessage $ ExpandTowards R)
    , ("M-M1-h", sendMessage $ ExpandTowards L)
    , ("M-M1-j", sendMessage $ ExpandTowards D)
    , ("M-M1-k", sendMessage $ ExpandTowards U)
    , ("M-M1-C-l", sendMessage $ ShrinkFrom R)
    , ("M-M1-C-h", sendMessage $ ShrinkFrom L)
    , ("M-M1-C-j", sendMessage $ ShrinkFrom D)
    , ("M-M1-C-k", sendMessage $ ShrinkFrom U)
    , ("M-r", sendMessage $ Rotate)
    , ("M-s", sendMessage $ Swap)
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

-- Most preferred installed terminal emulator.
myTerminal :: IO String
myTerminal = firstInstalled
  [ "alacritty"
  , "urxvtc"
  , "urxvt"
  , "xterm"
  ]

firstInstalled :: [String] -> IO String
firstInstalled cmds = fromJust <$> firstM isInstalled cmds
  where
    firstM f [] = return Nothing
    firstM f (m:ms) = do
      ok <- f m
      if ok then
        return $ Just m
      else
        firstM f ms
    isInstalled cmd = isJust <$> findExecutable cmd

main :: IO ()
main = do
  spawn "xscreensaver"

  -- favorite installed terminal emulator
  myTerminal' <- myTerminal
  myBindings' <- myBindings

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
    { modMask           = mod4Mask
    , manageHook        = myManageHook <+> manageHook def
    , terminal          = myTerminal'
    , layoutHook        = (workspaceDir "~" . smartBorders) myLayoutHook
    , focusFollowsMouse = False
    , startupHook = liftIO $ hPutStrLn h ""
    } `additionalKeysP` myBindings'
  >>= myXmobar "xmobar ~/.xmonad/xmobarrc-top"
  >>= xmonad . ewmh
  where
    myLayoutHook
      = emptyBSP ||| (named "Tabbed" . tabbed shrinkText) def
