;;-----------------------------------------------------------------
;; Color Theme
;;-----------------------------------------------------------------
;; Add the color theme directory and theme directory to the load
;; path
(add-to-list 'load-path (concat packages-dir "/color-theme"))
(add-to-list 'load-path (concat dotfiles-dir "/themes"))

;; Initialize color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(setq color-theme-is-cumulative t)
(setq color-theme-load-all-themes nil)

