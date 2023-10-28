;; add this to your init.el
;; (load-file "~/.emacs.d/telega-misc/notification.el") 
;;


(load-file "~/.emacs.d/telega-misc/alert.el")
(load-file "~/.emacs.d/telega-misc/telega_alert.el")

;; telega-alert-activation.el

(setq alert-default-style 'libnotify)

(defvar telega-alert-activation-enabled t)

(defun activate-telega-alert-mode ()
  (when (and telega-alert-activation-enabled
             (string-match-p "telega" (buffer-name)))
    (telega-alert-mode 1)))

(add-hook 'buffer-list-update-hook 'activate-telega-alert-mode)
