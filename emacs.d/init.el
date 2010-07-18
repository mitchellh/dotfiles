;;-----------------------------------------------------------------
;; Default Variables (Global)
;;-----------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default tab-width 2)
(setq-default espresso-indent-level 2)
(setq-default vc-handled-backends nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;;-----------------------------------------------------------------
;; Load paths and folder variables
;;-----------------------------------------------------------------
;; Variables to help
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq packages-dir (concat dotfiles-dir "/packages"))

;; Add this directory to load path initially
(add-to-list 'load-path dotfiles-dir)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
(setq auto-save-default nil)

;;-----------------------------------------------------------------
;; Display Options
;;-----------------------------------------------------------------
(load "display.el")

(if window-system
    (load "window.el")
    (load "terminal.el"))

;;-----------------------------------------------------------------
;; Other Packages and Files
;;-----------------------------------------------------------------
(load "modes.el")
(load "keybindings.el")
(load "hooks.el")
(load "filetypes.el")

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;;-----------------------------------------------------------------
;; System/User-specific configuration
;;-----------------------------------------------------------------
;; Loads system-type config first. e.g. "darwin.el" on mac
(let ((system-type-config (concat dotfiles-dir (symbol-name system-type) ".el")))
  (if (file-exists-p system-type-config)
    (load system-type-config)))

;; Load user-specific config next
(let ((user-login-config (concat dotfiles-dir user-login-name ".el")))
  (if (file-exists-p user-login-config)
    (load user-login-config)))

(provide 'init)
