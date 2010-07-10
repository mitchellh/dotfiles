;;-----------------------------------------------------------------
;; js-mode (espresso)
;;-----------------------------------------------------------------
;; Espresso mode has sane indenting so we use that.
(setq js-indent-level 2)

;;-----------------------------------------------------------------
;; JS2-Mode
;;-----------------------------------------------------------------
;; Set path
(add-to-list 'load-path (concat packages-dir "/js2-mode"))
(autoload 'js2-mode "js2" nil t)

;; Customize
(setq js2-basic-offset 2)
(setq js2-cleanup-whitespace t)
