;;-----------------------------------------------------------------
;; Hooks
;;-----------------------------------------------------------------
;; There is no reason to ever have trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)