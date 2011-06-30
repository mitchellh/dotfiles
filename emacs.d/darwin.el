;;-----------------------------------------------------------------
;; Windowed Mode Options
;;-----------------------------------------------------------------
;; We only do this on Mac since on Linux this is controlled via the
;; XRDB resource files.
(if window-system
    (progn (tool-bar-mode -1)
           (toggle-scroll-bar -1)))
(menu-bar-mode -1)

;;-----------------------------------------------------------------
;; Mac Specific Configuration
;;-----------------------------------------------------------------
;; The command key should be the meta key
(setq mac-command-modifier 'meta)

;; Fullscreen is a bit different on OS X
(global-set-key (kbd "M-n") 'ns-toggle-fullscreen)

;; Copy and Paste
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Override defaults to use the mac copy and paste
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Mac-style save/undo
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)