import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar def
  { modMask = mod1Mask
  , terminal = "urxvt"
  }
