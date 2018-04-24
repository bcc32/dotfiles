import XMonad

main = xmonad $ defaultConfig
  { modMask = mod1Mask
  , terminal = "urxvt"
  }
