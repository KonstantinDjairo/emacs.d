;;;; add inactive timestamp to every org-item
;; https://stackoverflow.com/a/52815573/5115219
;; https://emacs.stackexchange.com/a/45369/29404
(defun insert-created-date (&rest ignore)
  "Insert inacative timestamp property,
but only in org-items, not in org-item-checkboxes."
  (interactive)
  (if (not (org-at-item-checkbox-p))
      (progn
        (insert (format-time-string
                 (concat "\n"
                         ":properties:\n"
                         ":created: "
                         "[%Y-%m-%d %a %H:%M]\n"
                         ":end:"
                         )))
        ;; in org-capture, this folds the entry; when inserting a heading, this moves point back to the heading line
        (org-back-to-heading)
        ;; when inserting a heading, this moves point to the end of the line
        (move-end-of-line ()))))
;;;; add inactive timestamp to  entries in org-mode
(advice-add 'org-insert-heading :after #'insert-created-date)

;; ----------------------------------------------------------------------
;;;                          org-roam
;; ----------------------------------------------------------------------
;;;; Filter nodes
;; https://www.reddit.com/r/emacs/comments/vb989o/orgroam_show_only_file_nodes_when_inserting_a_node/ic8bc7l/?context=3

;;;;; Define regexp for filtering daily journal files like `2022-05-22'
(setq my-date-regexp "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+")

;;;;; Define filter functions to be used in `org-roam-node-find' function.
(defun ugt-filter-org-roam-node-file-p (node)
  "Filter nodes that represent files.
So exclude nodes that are outline items in org files.

Usage example:
(org-roam-node-read nil #'ugt-filter-org-roam-node-file-p)
"
  (and
   (= (org-roam-node-level node) 0)
   (not (string-match my-date-regexp (org-roam-node-title node)))))

(defun ugt-filter-org-roam-node-exclude-dates (node)
  "Exclude journal files like `2022-05-17' from nodes list."
  (not (string-match my-date-regexp (org-roam-node-title node))))

(defun ugt-filter-org-roam-node-exclude-archived-and-journal-files (node)
  "Exclude these files / nodes
- tagged `archive'
- in folder `archive'
- journal files."
  (and
   ;; no journal files
   (not (string-match my-date-regexp (org-roam-node-title node)))
   ;; not tagged `archive'
   (not (member "archive" (org-roam-node-tags node)))
   ;; not in any folder named `archive'
   (not (string-match-p "archive/" (org-roam-node-file node)))))

;;;;; Define custom `org-roam-node-find' functions with filters.
(defun ugt-org-roam-node-find nil
  "Refined search for org-roam nodes.
Exclude elements tagged `archive'."
  (interactive)
  ;; nb: can add initial search string like "^"
  (org-roam-node-find :other-window nil #'ugt-filter-org-roam-node-exclude-archived-and-journal-files))

(defun ugt-org-roam-node-find-document-nodes nil
  "Refined search for org-roam nodes.
Search for only document level nodes. Exclude dates."
  (interactive)
  ;;(org-roam-node-find :other-window)
  (org-roam-node-find :other-window nil #'ugt-filter-org-roam-node-file-p))

;;;; Custom `org-roam-dailies-goto-today' function
(defun ugt-org-roam-dailies-goto-today nil
  "Open todays journal in other window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (org-roam-dailies-goto-today))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n f" . org-roam-node-find)
         ("M-o" . ugt-org-roam-node-find)
         ("M-O" . ugt-org-roam-node-find-document-nodes)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         ("s-l" . org-roam-buffer-toggle)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("s-j" .     org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("s-t"     . ugt-org-roam-dailies-goto-today)
         ("C-s-["   . org-roam-dailies-goto-previous-note)
         ("C-s-]"   . org-roam-dailies-goto-next-note)
	 (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("s-i" .     org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)
                ("C-c n g" . org-roam-graph))))
  :config
  ;;(org-roam-setup)
  (setq org-roam-node-display-template
        (concat "${title:70}"(propertize "${tags:30}" 'face 'org-tag) "${file:48}"))
  (setq org-roam-capture-templates '(
                                     ("d" "default (personal notes)"
                                      plain "%?"
                                      ;; could use
                                      ;; (file (concat org-directory "/org-roam/personal/templates/personal.org"))
	                              :if-new (file+head "personal/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
	                              :unnarrowed t)

                                     ("q" "FreeBSD"
                                      plain "%?"
                                      :if-new (file+head "personal/freebsd/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: FreeBSD\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)

                                     ("l" "Japanese"
                                      plain "%?"
                                      :if-new (file+head "personal/japanese/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: Japanese\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)

				     
				     
                                     ("z" "Blog Post"
                                      plain "%?"
                                      :if-new (file+head "~/omnia/weblog/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: blog_post\n#+startup: showall\n
                                                          #+AUTHOR: Alexandro Longo \n
                                                          #+HTML_HEAD_EXTRA: <style>*{font-size: medium;}</style> \n
                                                          #+OPTIONS: toc:nil \n
                                                          #+HTML_HEAD: <link rel="stylesheet" type="text/css" href="fix/morris.css">\n
                                                          \n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)				     
				     
                                     ("c" "Math Ideas"
                                      plain "%?"
                                      :if-new (file+head "personal/math-ideas/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: mathematical_ideas\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)
				     
                                     ("t" "Proofs"
                                      plain "%?"
                                      :if-new (file+head "personal/math-ideas/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: mathematical_proofs\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)
				     
                                     ("x" "Real Analysis"
                                      plain "%?"
                                      :if-new (file+head "personal/math-ideas/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: real_analysis\n#+startup: showall\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t)

				     
                                     ("n" "My Readings"
                                      plain "%?"
                                      :if-new (file+head "my-readings/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: My_readings \n#+startup: content\n")
                                      :empty-lines 1
                                      :unnarrowed t)

				     
                                     ("m" "Emacs related notes"
                                      plain "%?"
                                      :if-new (file+head "emacs/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: emacs\n#+startup: content\n")
                                      :empty-lines 1
                                      :unnarrowed t)

                                     ("w" "work notes"
                                      plain "%?"
                                      :if-new (file+head "work/${slug}.org"
                                                         "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+updated: \n %R>\n#+filetags: work_notes\n\n")
                                      :immediate-finish t
                                      :empty-lines 1
                                      :unnarrowed t))
        time-stamp-start "#\\+updated: [\t]*")
  (setq org-roam-dailies-capture-templates  '(("d" "default" entry
                                                 "\n* %<%H:%M> %?\n:properties:\n:created: %U\n:end:\n"
                                                 :target  (file+head "%<%Y-%m-%d>.org"
                                                                     "#+title: %<%Y-%m-%d %a>\n#+startup: showall\n")
                                                 ;;:unnarrowed t
                                                 ;;:jump-to-captured t
                                                 :empty-lines 1
                                                 )))
  ;; Change file-name (slug) creation
  ;; Replace whitespace with dashes instead of underscores.
  ;; See
  ;; - https://github.com/org-roam/org-roam/issues/686
  ;; - https://github.com/org-roam/org-roam/pull/1544[[id:2022-06-12T213159.588064][test mest hest]]
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                        ("--*" . "-")                   ;; remove sequential underscores
                        ("^-" . "")                     ;; remove starting underscore
                        ("-$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))
  ;; https://org-roam.discourse.group/t/v2-set-id-to-a-timestamp/1492/2
  ;; (setq org-roam-capture-templates
  ;;       '(("p" "personal" plain
  ;;          (function org-roam--capture-get-point) "%?"
  ;;          :file-name "personal/%<%Y-%m-%dT%H%M%S>"
  ;;          :head "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S.%6N">\nmodified: <>\n---\n"
  ;;          :unnarrowed t)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;;; Go to item text after capture
(defun ugt-org-capture-after-finalize-hook nil
  (org-capture-goto-last-stored)
  (end-of-buffer)
  (recenter-top-bottom '(4)) ;; center of window
  )
(add-hook 'org-capture-after-finalize-hook #'ugt-org-capture-after-finalize-hook)

;;;; Hide drawers after creation
(defun my-org-capture-hook ()
  (when (plist-member org-capture-plist :org-roam)
    (org-cycle-hide-drawers 'overview)))
(add-hook 'org-capture-mode-hook #'my-org-capture-hook)

;;;; Hide drawers in all cases in org-roam.
(defun my-org-roam-hook () (org-cycle-hide-drawers 'overview))
(add-hook 'org-roam-mode-hook #'my-org-roam-hook)

;;;; Jump to the end of buffer when today's journal is opened
(defun ugt-org-roam-dailies-find-file-hook nil (end-of-buffer))
(add-hook 'org-roam-dailies-find-file-hook #'ugt-org-roam-dailies-find-file-hook)

;;;; add inactive timestamp to journal entries in org-roam
(add-hook 'org-roam-dailies-capture-today #'insert-created-date)

;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-roam-ui - graphs
;; https://github.com/org-roam/org-roam-ui
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
(defun ugt-counsel-rg ()
  "Custom `counsel-rg' function."
  (interactive)
  (let ((initial-input "^\\*+ ") ;; prefill search with regexp searching for lines starting with `*'
        (initial-directory "~/org") ;; Search in
        ;; Exclude folders `Backups' and `Apps'; show long lines
        (extra-rg-args "-g!#* -g!Backups/* -g!Apps/* --max-columns 600") 
        (rg-prompt "rg: Search org file headers (narrow with =S-SPC= or =!keyword=: "))
    (counsel-rg initial-input
                initial-directory
                extra-rg-args
                rg-prompt)))
(global-set-key (kbd "s-u") 'ugt-counsel-rg) ;; Map to `CMD + u' on a Mac


(defun ugt-counsel-rg-el-files ()
  "Search the whole .emacs.d folder for code snippets / functions etc"
  (interactive)
  (let ((initial-input "^[^;]+ ") ;; ingnore comment lines
        (initial-directory "~/.emacs.d/")
        (extra-rg-args "--max-columns 500")
        (rg-prompt "rg: Search .el files: "))
    (counsel-rg initial-input
                initial-directory
                extra-rg-args
                rg-prompt)))
;; Map to `CMD + .' on a Mac
(global-set-key (kbd "s-.") 'ugt-counsel-rg-el-files)

;; ------------------------------------------------------------------------
;;;     search in various folders via counsel-fzf
;; ------------------------------------------------------------------------
;;;; helper functions
(defun ugt-replace-in-string (search replace-with in-string)
  (replace-regexp-in-string (regexp-quote search) replace-with in-string nil 'literal))

(defun ugt-expand-tilde-in-path (path)
  "Replace tilde in paths with full path.
So `~/.emacs.d' becomes `/Users/pragmat1c1/.emacs.d'"
  (let ((search "~")
        (replace-with (getenv "HOME")))
    (cond ((string-match-p search path)
           (ugt-replace-in-string search replace-with path))
          (t
           path))))

(defun ugt-file-in-path-p (file-path check-path)
  "Check if FILE-PATH is in CHECK-PATH."
  (string-prefix-p (ugt-expand-tilde-in-path check-path)
                   (ugt-expand-tilde-in-path file-path)
                   :ignore-case))

(defun ugt-counsel-fzf (path)
  ;(other-window 1)
  (counsel-fzf nil
               path
               (format "Search via `counsel-fzf' in path `%s': " path)))

(defun ugt-counsel-fzf-org-files ()
  "Open files in dir of current file."
  (interactive)
  (let* ((path org-directory))
    (ugt-counsel-fzf path)
    (other-window 1)
    ;;(org-modern-mode)
    ;;(olivetti-mode)
    ))

(global-set-key (kbd "s-O") 'ugt-counsel-fzf-in-path-of-current-file)
(global-set-key (kbd "s-o") 'ugt-counsel-fzf-org-files)
(global-set-key (kbd "C-c o") 'counsel-projectile-find-file)

