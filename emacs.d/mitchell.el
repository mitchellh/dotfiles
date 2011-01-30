;;-----------------------------------------------------------------
;; Color Theme
;;-----------------------------------------------------------------
;; Only enable this when a windowing system is available
(if window-system
    (progn (require 'color-theme-tango)
           (require 'color-theme-tangotango)
           (color-theme-tangotango)))
