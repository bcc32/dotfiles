import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar defaultConfig
  { modMask = mod1Mask
  , terminal = "urxvt"
  }
