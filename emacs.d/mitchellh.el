;;-----------------------------------------------------------------
;; Color Theme
;;-----------------------------------------------------------------
;; Only enable this when a windowing system is available
(if window-system
    (progn (require 'color-theme-tango)
           (require 'color-theme-tangotango)
           (color-theme-tangotango)))

;;-----------------------------------------------------------------
;; ERC
;;-----------------------------------------------------------------
(defun my-erc-connect ()
  (interactive)
  (progn (erc :server "irc.freenode.net" :port 6667 :nick "mitchellh")
         (erc :server "irc.citrusbyte.com" :port 6667 :nick "mitchellh" :password "4pk4cUyb7NL14L4moom9ledwVIoXfo1")))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#vagrant" "#chef" "#cbzd" "#rubinius")
        ("citrusbyte.com" "#cb")))
