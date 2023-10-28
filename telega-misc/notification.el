;; add this to your init.el
;; (load-file "~/.emacs.d/telega-misc/notification.el") 
;;

(defvar alert-default-style 'libnotify)
(load-file "~/.emacs.d/telega-misc/alert.el")
(require 'alert)
(load-file "~/.emacs.d/telega-misc/telega_alert.el")
(require 'telega-alert)
;; telega-alert-activation.el

;; test
;; (alert "This is an alert" :title "My Alert") 
;;


(telega-alert-mode 1)
