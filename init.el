;; -*- mode:read-only; mode:emacs-lisp -*-

;; clean all pre-compiled binaries (elc) from your emacs folder
;; find . -name "*.elc" -type f | xargs rm -f
;; --------------------------------------------

(load-file "/home/ronnie/.emacs.d/typing/typing-speed.el")
(load-file "/home/ronnie/.emacs.d/typing/key-guide.el")
(turn-on-typing-speed)
(defun enable-typing-speed-mode ()
  "Enable typing-speed-mode in the current buffer."
  (unless (bound-and-true-p typing-speed-mode)
    (typing-speed-mode)))

(defun setup-typing-speed-for-buffer ()
  "Setup typing-speed-mode for the current buffer when it's created."
  (add-hook 'after-make-frame-functions 'enable-typing-speed-mode)
  (add-hook 'buffer-list-update-hook 'enable-typing-speed-mode))

(setup-typing-speed-for-buffer)

(setq warning-minimum-level :emergency) 





(load-file "/home/ronnie/.emacs.d/typing/wakib-keys.el")
(wakib-keys 0)


;; japanese fonts with anti-alising
(defun enable-antialiasing ()
  "Enable antialiasing in Emacs by customizing faces with the 'PlemolJP' font."
  (interactive)
  (custom-set-faces
   '(default ((t (:height 120 :foundry "ADBO" :family "PlemolJP" :antialias t))))
   '(font-lock-comment-face ((t (:foreground "dark green" :weight normal :slant italic :antialias t))))
   '(font-lock-keyword-face ((t (:weight bold :antialias t))))
   '(font-lock-string-face ((t (:foreground "dark magenta" :antialias t))))
   ;; Add more face customizations as needed
   )
  (message "Antialiasing enabled with 'PlemolJP' font for selected faces."))

(enable-antialiasing)
(global-set-key [f9] 'enable-antialiasing)
;; ----------------------------




(load-file "/home/ronnie/.emacs.d/elisp-bug-hunter/bug-hunter.el")

;; /home/ronnie/.emacs.d/telega-misc/visual-fill-column.el
(load-file "~/.emacs.d/infinite/infinite.el")

;;
(add-to-list 'load-path "~/.emacs.d/neotree/")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)



;; clean interface
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))




;;(load-file "/home/ronnie/.emacs.d/colophon/main.el")


(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))



(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(require 'quelpa)
(setq quelpa-update-melpa-p nil)



(package-install 'quelpa-use-package)
(require 'quelpa-use-package)


;; theme
(use-package tron-legacy-theme
  :config
  (setq tron-legacy-theme-vivid-cursor t)
  (load-theme 'tron-legacy t))


(add-to-list 'load-path "/home/ronnie/.emacs.d/dashboard/emacs-dashboard/")
(require 'dashboard)
(dashboard-setup-startup-hook)

(enable-antialiasing)

;; Set the title
(setq dashboard-banner-logo-title "いらっしゃいませ~!")
;; Set the banner
(setq dashboard-startup-banner "/home/ronnie/.emacs.d/dashboard/cute_girl.png")
;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

;; planning
;;(load-file "/home/ronnie/.emacs.d/my-org-roam-elisp-code/org-hyperscheduler/org-hyperscheduler.el")


;; refactor this terrible code, to call org-noter from inside .emacs.d
(use-package org-noter
    :after org
    :ensure t
    :config (setq org-noter-default-notes-file-names '("notes.org")
                  org-noter-notes-search-path '("~/org/Research-Notes")
                  org-noter-separate-notes-from-heading t))


(require 'pdf-tools)
(pdf-tools-install) ; Standard activation command

;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)
;; use normal isearch
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)


;; my deep learning book
(defun my/hide-mode-line-for-pdf-buffers ()
  "Hide mode line for buffers with filenames containing 'pdf'.
Show only the part after 'pdf'."
  (when (and buffer-file-name
             (string-match ".*pdf\\(.*\\)" buffer-file-name))
    (setq mode-line-buffer-identification
          (substring buffer-file-name (match-beginning 1)))))

(add-hook 'after-change-major-mode-hook 'my/hide-mode-line-for-pdf-buffers)



(defun my-open-pdf-and-call-function ()
  "Open a PDF file and call another function after it's opened."
  (interactive)
  (let ((pdf-file "/mnt/Data/Documents/deep_learning/(Adaptive Computation and Machine Learning series) Ian Goodfellow, Yoshua Bengio, Aaron Courville - Deep Learning-The MIT Press (2016).pdf")) ; Change this to the path of your PDF file
    (if (get-buffer pdf-file)
        (progn
          (switch-to-buffer pdf-file)
          (pdf-view-mode)
          (my-another-function))
      (find-file pdf-file)
      (pdf-view-mode)
      (run-at-time
       "0.5 sec" nil
       (lambda ()
         (org-noter
	  pdf-misc-size-indication-minor-mode))))))



(global-set-key (kbd "C-c p") 'my-open-pdf-and-call-function) ; Bind to Ctrl+c p



;; telega pre-defining variables
;; deps
(load-file "~/.emacs.d/telega-misc/visual-fill-column.el")
(load-file"~/.emacs.d/telega-misc/rainbow-identifiers.el")
;; using images for displaying emojis is a terrible idea
(setq-default telega-emoji-use-images nil)

;; animations
;; dep : https://github.com/zevlg/tgs2png
(setq-default telega-sticker-animated-play t)

(setq-default telega-emoji-font-family "Apple Color Emoji")


;; telega will be called with quelpa later on; scroll down.
;; --------------------------------------------------------

;; Latex async preview
(load-file "~/.emacs.d/latex/org-preview.el")
;;(require 'org-preview)

;;(server-start)

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x k") 'nuke-all-buffers)

;; ======================================================
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)
;; ======================================================


(defun set-emoji-font ()
  (interactive)
  (set-fontset-font t '(#x1f000 . #x1ffff) "Apple Color Emoji"))

(set-emoji-font)

;; test 😍👍

;; Set Japanese font at startup
;; that code takes the range for japanase
(set-fontset-font t '(#x3040 . #x9fbf) "PlemolJP")

;; russian fonts
(set-fontset-font t '(#x0400 . #x04FF) "Victor Mono")
;; --------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/spaceway/")
(load-theme 'modus-operandi t)
(set-face-attribute 'default nil :height 90)
(enable-antialiasing)
;; 
(require 'package)
(add-to-list 'load-path "~/.emacs.d/use-package/use-package/")
(require 'use-package)
;;


;; My org-roam setup

;; org-mode
;;(optional)  you need to install "copy-as-org"
;; https://github.com/kuanyui/copy-as-org-mode
(require 'org)

(quelpa 'org-persist)

(require 'org-capture)


(load-file "~/.emacs.d/my-org-roam-elisp-code/org-roam-custom.el")



;; straight.el (dependencie of org-roam-ui)

(setq straight-repository-branch "develop")

(setq native-comp-async-report-warnings-errors nil)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)


;; timeblocking

(use-package org-timeblock
  :straight (org-timeblock :type git
              :host github
              :repo "ichernyshovvv/org-timeblock"))
(require 'compat)
(setq org-agenda-files '("~/org/inbox.org"))

;; org
(straight-use-package 'org)

(quelpa 'org-roam)
(require 'org-roam)

;; org-roam-ui
;;https://raw.githubusercontent.com/ahyatt/emacs-websocket/main/websocket.el
(load-file "/home/ronnie/.emacs.d/my-org-roam-elisp-code/websocket.el")


(require 'websocket)
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;----------------------------------------------------------------

;; Install Ement.
(use-package ement
  :straight (:host github :branch "main" :repo "alphapapa/ement.el"))


(defun Buffer-menu-fontify-and-adjust-frame ()
  "Use for `buffer-menu-mode-hook'.  Fontify, fit and raise frame."
  (save-window-excursion
    (save-excursion
      (pop-to-buffer "*Buffer List*")
      (when (< emacs-major-version 21)
        (make-local-variable 'font-lock-defaults))
      (setq font-lock-defaults 
            '(buffer-menu-font-lock-keywords t))
      (when (fboundp 'font-lock-refresh-defaults)
        (font-lock-refresh-defaults)) ; WHY NEEDED NOW?
      (turn-on-font-lock)
      (when (and (fboundp 'fit-frame) (one-window-p t))
        (fit-frame))
      (raise-frame))))


(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(global-set-key [f7] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)


;; utf-8
(prefer-coding-system 'utf-8)


;; change font size
(set-face-attribute 'default nil :height 90)
;; also a function and a keybinding to enable this in emergency case
(global-set-key (kbd "C-0") (set-face-attribute 'default nil :height 90))


;; improve emacs performance 
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)


;; Startup performance
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;-------------------------------------------------

;; when reading manga (in image format) i'd like to have a easy way to go to the next file (or page)
;;(defun find-next-file (&optional backward)
;;  "Find the next file (by name) in the current directory.

;;With prefix arg, find the previous file."
;;  (interactive "P")
;;  (when buffer-file-name
;;    (let* ((file (expand-file-name buffer-file-name))
;;           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
;;                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
;;           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
;;                     (length files))))
;;      (find-file (nth pos files)))))
;--------------------------------------


;; COMPILE EMACS WITH ATHENA TOOLKIT !

;; damn, i hate those warnings everytime!
(setq warning-minimum-level :emergency)
;; ------------------------------------

;; change themes without making emacs just load every fucking theme
(defadvice load-theme (before disable-before-load)
"Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
(dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(ad-activate 'load-theme)
;;----------------------------






;; Byte Compile All the emacs directory from command line
;; emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
;; -------------------------------------------------------------

(setq debug-on-error nil)


;;(set-face-font 'default "Dank Mono:style=Regular")
;;(set-default-font "DankMono Regular 12" nil t)
;;(set-face-attribute 'default nil :font "Dank Mono")



;;(defvar crz/font "Dank Mono")

;;(set-face-attribute 'default nil :font crz/font)
;;(set-face-attribute 'fixed-pitch nil :font crz/font)
;;(set-face-attribute 'variable-pitch nil :font crz/font)

;;(set-face-attribute 'default nil :height 125)


;;(defvar crz/font "Dank Mono")

;;(defun crz/set-font-faces ()
;;  (set-face-attribute 'default nil :font crz/font)
;;  (set-face-attribute 'fixed-pitch nil :font crz/font)
;;  (set-face-attribute 'variable-pitch nil :font crz/font))

;;(if (daemonp)
;;    (add-hook 'after-make-frame-functions
;;              (lambda (frame)
;;                (with-selected-frame frame (crz/set-font-faces))))
;;  (crz/set-font-faces))

;; ---------------------------------


(defun my/set-font ()
  "Set the default font for Emacs."
  (condition-case ex
      (set-frame-font "Dank Mono-15" nil t)
    (error (message "Failed to load font: %s" ex)
           ;; Fallback font configuration
           (set-face-attribute 'default nil
                               :family "Monospace"
                               :height 90
                               :weight 'normal
                               :width 'normal))))

;; Call the font setting function
(my/set-font)


(split-window-right)


(enable-antialiasing)



;;(if (daemonp)
;;    (add-hook 'after-make-frame-functions
;;              (lambda (frame)
;;                (with-selected-frame frame
;;                  (efs/set-font-faces))))
;;    (my/use-font))


;;(trace-function
;;  #'call-process nil
;;  (lambda ()
;;   (format "\nload-file-name: %s" load-file-name)))


;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")
			 ;; without this, transient package should not
			 ;; be listed and therefore matrix client will not work
			 ("elpa"   . "https://elpa.gnu.org/packages/")))

;; Update local list of available packages:
;; Get descriptions of all configured ELPA packages,
;; and make them available for download.
;;(package-refresh-contents)


;; Automatic pacakge update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))
;;------------------------------------------





(quelpa '(telega :fetcher github
               :repo "zevlg/telega.el"
               :branch "master"
               :files (:defaults "contrib" "etc" "server" "Makefile")))


(setq telega-server-libs-prefix "/usr/local/")
;;
;; temporary disabled, need to focus on what's within myself.
;;(require 'telega)
;;


(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))




;; org-noter-plus


(quelpa 'vterm)
(global-set-key [f3] 'vterm)

;; elfeed
;; TODO way of making backups of the information that i receive in rss feeds
(quelpa 'elfeed)
(setq elfeed-feeds
      '("https://arxiv.org/rss/math.CA/recent/"  ;; <-- my research field, this rss will keep me updated
	"https://rss.app/feeds/7YUuf2mmeudgvZVQ.xml"
	"https://tatsumoto.neocities.org/blog/feed.rss"
	"https://www.reddit.com/r/math/.rss"
	"https://scholar.archive.org/feed/rss?q=zfs+file+system"
	"http://nullprogram.com/feed/"
	"http://planetlarry.org/atom.php"
	"https://security.gentoo.org/glsa/feed.rss"
	"https://blog.tecosaur.com/tmio/rss.xml"
	"https://www.reddit.com/r/emacsng/.rss"
	"https://gpo.zugaina.org/RSS/RepNews"
	"https://gitweb.gentoo.org/repo/gentoo.git/atom/sys-apps/portage?h=master"
	"https://www.reddit.com/r/OrgRoam/.rss"
	"https://www.reddit.com/r/orgmode/.rss"
	"http://www.loper-os.org/?feed=rss2"
	"https://whhone.com/index.xml"
	"https://rss-bridge.snopyta.org/?action=display&bridge=Telegram&username=%40lambdareadingroom&format=Mrss"
	"https://sachachua.com/blog/feed"
	"https://esoteric.codes/rss"
	"https://www.trinitydesktop.org/rss.php"
        "https://planet.emacslife.com/atom.xml"
	;; julia
	"https://www.juliabloggers.com/feed/"
	"https://julialang.org/feed.xml"
	"https://juliacomputing.com/feed.xml"
	))
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)



(setq-default telega-emoji-font-family "Noto Color Emoji")


;;  to show images
(use-package image
  :custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  (image-use-external-converter t))

;; rust
(setq rustic-analyzer-command  '("/usr/bin/rust-analyzer-bin-1.65.0"))
(setq rustic-cargo-bin  '("/usr/bin//usr/bin/cargo-bin-1.65.0"))
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(with-eval-after-load 'lsp
  (with-eval-after-load 'rustic
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (add-hook 'rustic-mode-hook #'lsp-rust-analyzer-inlay-hints-mode)
    ))
(add-hook 'lsp-after-open-hook (lambda ()
                                 (when (lsp-find-workspace 'rust-analyzer nil)
                                   (lsp-rust-analyzer-inlay-hints-mode))))
(quelpa 'rustic)
(require 'rustic)
(require 'lsp-mode)


;; as you can see in trying hard to enable this thing
(with-eval-after-load 'org
   (require 'org-habit))

;; my book project
;;(add-to-list 'org-publish-project-alist
;;         '("My Book on real analysis"
;;        :base-directory "~/Book"
;;        :publishing-directory "~/Book/Publish"
;;        :publishing-function org-latex-publish-to-latex
;;        :body-only t
;;        :makeindex t
;;        ))
;; --------------------------------------------

;; my pomodoro setup using org
(setq org-clock-sound "~/.emacs.d/my-org-roam-elisp-code/sound-clock/sound.wav")

;; configuring emacs to ask before quiting 
;; why it's not built in???
(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)                                                                                            
    (message "Canceled frame close")))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))
;;----------------------------------------------------------------


;; each state with ! is recorded as state change
;; in this case I'm logging TODO and DONE states
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n)" "SOMD(s)" "WAFO(w)" "|" "DONE(d!)" "CANC(c!)")))
;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)
;; log into LOGBOOK drawer
(setq org-log-into-drawer t)
;; ------------------------------------------------------------------

;; configuring org-mode to serve as word-processor
(setq org-hide-emphasis-markers t)

;; ------------------------------------------------------------



;; Mind Map generator with Org-Mode !
;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(quelpa 'ox-org)
(require 'ox-org)

;; quarto 
(quelpa 'quarto-mode)
;; load the library
(require 'quarto-mode)

(use-package org-mind-map
  :init
  :quelpa
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )
;; #############################################################
(require 'org-mind-map)

;; this package sucks a lot
(setq org-excluded-packages '(org-bullets))



(load-file "~/.emacs.d/telega-misc/rainbow-identifiers.el")

;; Org-noter (pdf-anotation)

(load-file "~/.emacs.d/autoload/org-noter/org-noter.el")

;; org-notion.el
(load-file "~/.emacs.d/org-notion/org-notion/org-notion.el")
(require 'org-notion)



;; BLOG
(load-file "~/.emacs.d/colophon/main.el")

;;  (setq org-roam-dailies-capture-templates
;;               '(("m" "main" plain
;;           "* %<%H:%M>\n%?"
;;           :if-new (file+head file+head "%<%Y-%m-%d>.org"
;;			      "#+title: %<%Y-%m-%d>"
;;			      "#+FILETAGS: :DailyNote:Journaling")
;;           :immediate-finish t
;;           :unnarrowed t))
;; -------------------------------------


;; telegram client for emacs (telega.el)
;;(quelpa '(telega :fetcher github
;;	       :upgrade t
;;               :repo "zevlg/telega.el"
;;               :branch "master"
;;               :files (:defaults "contrib" "etc" "server" "Makefile")))
;; --------------------------------------------------------------------

;; org-notion
(quelpa 'ert-runner)
(quelpa 's)
(quelpa 'dash)

(load-file "~/.emacs.d/org-notion/org-notion/org-notion.el")
(require 'org-notion)



;; =================================


;;
(straight-use-package 'transient)



;; nano-emacs
(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(setq nano-font-size 17)




;; Matrix client for Emacs
;; Install and load `quelpa-use-package'.
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; Install Ement.
(use-package ement
  :straight (ement :type git :host github :repo "alphapapa/ement.el"
		   ;; this software is pretty new, so i will update everytime
		    :upgrade t))
;;----------------------------------------------------------------------


(use-package telega
  :straight (telega :type git :host github :repo "zevlg/telega.el" 
		   ;; this software is pretty new, so i will update everytime
		    :upgrade t))



;; it depends on pandoc
(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))

;; _______  _______  _______        _______  _______  ______   _______  _______  _       
;;(  ___  )(  ____ )(  ____ \      (       )(  ___  )(  __  \ (  ____ \(  ____ )( (    /|
;;| (   ) || (    )|| (    \/      | () () || (   ) || (  \  )| (    \/| (    )||  \  ( |
;;| |   | || (____)|| |            | || || || |   | || |   ) || (__    | (____)||   \ | |
;;| |   | ||     __)| | ____       | |(_)| || |   | || |   | ||  __)   |     __)| (\ \) |
;;| |   | || (\ (   | | \_  )      | |   | || |   | || |   ) || (      | (\ (   | | \   |
;;| (___) || ) \ \__| (___) |      | )   ( || (___) || (__/  )| (____/\| ) \ \__| )  \  |
;;(_______)|/   \__/(_______)      |/     \|(_______)(______/ (_______/|/   \__/|/    )_)
                                                                                 



;; -------------------------------------------------------------

(quelpa 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


;; Anki
(load-file "~/.emacs.d/anki-config/my-anki-config.el")

;; torrent client
;;(load-file "~/.emacs.d/autoload/torrent-client/aria2.el")


;; my file manager
;;(load-file "~/.emacs.d/autoload/dirvish/dirvish.el")
(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

;; using it as sidebar file searcher
;;(global-set-key [f8] 'dirvish-side)

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.8))



;;------------------------------------------------------

;; Markdown
(quelpa 'flymd)
(require 'flymd)
;; render as i type 
(quelpa 'impatient-mode)
(require 'impatient-mode)
(quelpa 'markdown-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.\\(post\\)\\'" . markdown-mode))

;;(add-to-list 'load-path "~/.emacs.d/autoload/emacs-application-framework/")
;;(require 'eaf)
;; eaf-markdown-previewer depends on a python package
;; called: python-markdown
;; to install it:  pip install markdown	
;;(require 'eaf-markdown-previewer)

(defun markdown-filter (buffer)
     (princ
       (with-temp-buffer
         (let ((tmpname (buffer-name)))
           (set-buffer buffer)
           (set-buffer (markdown tmpname)) ; the function markdown is in `markdown-mode.el'
           (buffer-string)))
       (current-buffer)))

;; matrix client
;; Install and load `quelpa-use-package'.
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)
;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))
;; Install Ement.
;;(use-package ement
;;  :quelpa (ement  :fetcher github :repo "alphapapa/ement.el"))


;; common lisp configuration
(quelpa 'slime)
;;; The SBCL binary and command-line arguments
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

(require 'slime)


;; kotlin
(quelpa 'kotlin-mode)
(require 'kotlin-mode)

;;  Python
;; Ensure Quelpa is installed and up to date
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install and load python-mode using Quelpa
(quelpa '(python-mode :fetcher github :repo "python-mode/python-mode"))
(require 'python-mode)

(add-hook 'python-mode-hook 'electric-indent-mode)
(add-hook 'python-mode-hook 'my-python-hook)

(defun my-python-hook ()
  (setq indent-tabs-mode t)  ; Use tabs for indentation
  (define-key python-mode-map (kbd "RET") 'newline-and-indent))

;; Install py-autopep8 package
(unless (package-installed-p 'py-autopep8)
  (package-install 'py-autopep8))

;; Configure py-autopep8
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)



;;;



;; writer configuration
(quelpa 'olivetti)
(quelpa 'writeroom-mode)



;; spaceway theme
(load-file "~/.emacs.d/themes/spaceway/spaceway-theme-autoloads.el")
(load-file "~/.emacs.d/themes/spaceway/spaceway-theme.el")




;;
;;

(use-package emacsql-sqlite
  :quelpa
  :ensure t)




(defadvice load-theme (before disable-before-load)
"Disable any loaded themes before enabling a new theme.
This prevents overlapping themes; something I would rarely want."
(dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(ad-activate 'load-theme)




;; =======================================



;; fonts

(set-language-environment "UTF-8")







;; ================================================
;;
;; I am using this function to kill all the buffers in emacs
;;


;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#ffffff" :family "Sans Serif")))))





;; ----------------
;;My 
;;____      _  __          
;; / ___|__ _| |/ _|_      __
;;| |   / _` | | |_\ \ /\ / /
;;| |__| (_| | |  _|\ V  V / 
;; \____\__,_|_|_|   \_/\_/  
;;configuration

(setq org-agenda-files '("~/org/agenda.org"))

(add-to-list 'load-path "~/.emacs.d/autoload/calfw/")

(load-file "~/.emacs.d/autoload/emacs-calfw/calfw.el")
(load-file "~/.emacs.d/autoload/emacs-calfw/calfw-org.el")
(load-file "~/.emacs.d/autoload/emacs-calfw/calfw-cal.el")

(require 'calfw)
(require 'calfw-org)

;; my keybind for org calendar in calfw
(global-set-key (kbd "C-/") 'cfw:open-org-calendar)
;; my keybind for my single org-agenda file 
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/org/agenda.org")))



(setq cfw:org-overwrite-default-keybinding t)

(cfw:create-calendar-component-region :height 20)
;; functions by Tarhuntas, for time inserting
(defun today ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))
(global-set-key (kbd "H-t") 'today)
(defun now-string ()
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
;; ###############################################################




;; Month
(setq calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])

;; Week days
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; First day of the week
(setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

;; Default setting
(setq cfw:fchar-junction ?+
      cfw:fchar-vertical-line ?|
      cfw:fchar-horizontal-line ?-
      cfw:fchar-left-junction ?+
      cfw:fchar-right-junction ?+
      cfw:fchar-top-junction ?+
      cfw:fchar-top-left-corner ?+
      cfw:fchar-top-right-corner ?+ )

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)
      
;; Another unicode chars
(setq cfw:fchar-junction ?╬
      cfw:fchar-vertical-line ?║
      cfw:fchar-horizontal-line ?═
      cfw:fchar-left-junction ?╠
      cfw:fchar-right-junction ?╣
      cfw:fchar-top-junction ?╦
      cfw:fchar-top-left-corner ?╔
      cfw:fchar-top-right-corner ?╗)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["gray35" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-safe-themes
   '("3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "ab058aa22bdaf17b5d8a9e21632a62c8966728ae10ef8fd07e95637e9cdf7a7b" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73" "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db" "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" "9259305fb97a25e8d9143797879509b85e9c394154a7f525badfb06e24c36c68" default))
 '(exwm-floating-border-color "#646464")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-changes-colors nil)
 '(highlight-changes-face-list '(success warning error bold bold-italic))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#c0c530")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae2f0")
     ("DONT" . "#70b900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#d3b55f")
     ("KLUDGE" . "#d0bc00")
     ("HACK" . "#d0bc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9077")
     ("XXX+" . "#ef8b50")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(mini-modeline-face-attr '(:background unspecified))
 '(org-src-block-faces 'nil)
 '(pdf-view-midnight-colors '("#ffffff" . "#100f10"))
 '(warning-suppress-log-types '((emacs)))
 '(widget-link-prefix "[")
 '(widget-link-suffix "]")
 '(widget-mouse-face '(highlight widget-button))
 '(widget-push-button-prefix "[")
 '(widget-push-button-suffix "]")
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#ef8b50" "#70b900" "#c0c530" "#79a8ff" "#f78fe7" "#4ae2f0" "white"]))


(set-face-attribute 'default nil :height 105)


(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(setq debug-on-error nil)



(setq inhibit-compacting-font-caches t)




