;;-----------------------------------------------------------------
;; Windowed Mode Options
;;-----------------------------------------------------------------
;; We only do this on Mac since on Linux this is controlled via the
;; XRDB resource files.
(if window-system
    (progn (tool-bar-mode -1)
           (toggle-scroll-bar -1)))
(menu-bar-mode -1)
