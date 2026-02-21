(defmacro comment (&rest body)
  "Comment out one or more s-expressions." nil)

(setq ring-bell-function 'ignore)
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(load "~/.emacs.d/wnh-secrets.el")

(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
  '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-history/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Set some defaults
(setq wnh/font-small 95)
(setq wnh/font-large 110)

(use-package emacs
  :config
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (blink-cursor-mode -1)
    (winner-mode 1)
    (show-paren-mode 1)
    (hl-line-mode 1)
    (global-display-line-numbers-mode 1)
    (global-hl-line-mode 1)
    (tab-bar-mode 1)
    (setq-default notmuch-search-oldest-first nil)

    (cond
     ;;;;; Framework - Linux ;;;;;
     ((string= (system-name) "frmwrk")
      (menu-bar-mode 1)
      (setq wnh/font-small 100)
      (setq wnh/font-large 110)
      (menu-bar-mode -1)
      (set-face-attribute 'default nil :family "DejaVu Sans")
      ;(load-theme 'solarized-light)
      (load-theme 'modus-operandi)

      (global-display-line-numbers-mode 0)
      ;(set-face-attribute 'default nil :family "DejaVu Sans")
      (setq-default line-spacing 0.6))

     ;;;;; X1 Carbon - Linux ;;;;;
     ((string= (system-name) "x1")
      (setq wnh/font-small 90)
      (setq wnh/font-large 110)
      (menu-bar-mode -1)
      (setq-default line-spacing 0.4))

     ;;;;; Work MacBook - MacOS ;;;;;
     ((string= system-type  "darwin")   ;"Wills-MBP.localdomain"
      (dolist (dir '("/Users/wharding/.nix-profile/bin"
		     "/opt/homebrew/bin"
		     "/Users/wharding/bin"
		     "/Users/wharding/work/bin"))
	(setq exec-path (cons dir exec-path))
	(setq user-mail-address "will@taskhuman.com")
	(setq user-full-name "Will Harding")
	(setenv "PATH" (concat dir ":" (getenv "PATH"))))

      (setq mac-command-modifier 'meta)
      (global-display-line-numbers-mode -1)
      ;(set-face-attribute 'default nil :family "Fira Sans")
      ;(set-face-attribute 'default nil :family "~/.nix-profile/share/fonts/truetype/iosevka-regular.ttf")
      (set-face-attribute 'default nil :family "Iosevka Term")
      (set-face-attribute 'default nil :height 80)
      (set-face-attribute 'default nil :height 120)
      ;(set-face-attribute 'tab-bar nil :height 160)
      ;(set-face-attribute 'default nil :family "Verdana")
      ;(set-face-attribute 'default nil :height 110)
      (setq wnh/font-small 110)
      (setq wnh/font-large 130)
      (setq-default line-spacing 0.8)
      (load-theme 'modus-operandi)))

    (setq indent-tabs-mode nil)

    (setq gc-cons-theshold 20000000
	  inhibit-splash-screen t
	  inhibit-startup-message t
	  show-paren-delay 0)

    (setq frame-title-format
	  '((:eval (if (buffer-file-name)
		       (abbreviate-file-name (buffer-file-name)))
		   "%b")))

    ;; Move temp files out of local directories and into system temp
    (setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))


(defun wnh/run-q-macro ()
  (interactive)
  (evil-execute-macro 1 (evil-get-register ?q)))

(defun wnh/comint-mongo-mirror ()
  (interactive)
  (comint-run "/Users/wharding/.nix-profile/bin/mongosh"
	      '("--quiet"
	      "mongodb://localhost/TaskHumanMirror")))

(defun wnh/capture-log ()
  (interactive)
  (org-capture :keys "L"))

(use-package evil
  :ensure t
  :init
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
  :config
    (evil-mode 1)

    (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-e") #'eval-defun)

    (evil-define-key 'normal js-mode-map (kbd "C-e") #'nodejs-repl-send-line)
    (define-key evil-visual-state-map (kbd "TAB") #'indent-region)

    (evil-define-key 'normal shell-mode-map (kbd "C-p") #'wnh/ctrl-p)
    (evil-define-key 'insert shell-mode-map (kbd "C-p") #'comint-previous-input)

    ;; Easier on the Mac where my Meta Key has changed
    (define-key evil-normal-state-map (kbd "C-x C-m") #'execute-extended-command)
    (define-key evil-normal-state-map (kbd "C-c C-m") #'execute-extended-command)
    (define-key evil-normal-state-map (kbd "SPC e") #'execute-extended-command)
    (define-key evil-normal-state-map (kbd "SPC b") #'switch-to-buffer)
    ;; TODO:put this in tehe eglot section?
    (define-key evil-normal-state-map (kbd "SPC l r") #'eglot-rename)
    (define-key evil-normal-state-map (kbd "SPC r e") #'recompile)
    (define-key evil-normal-state-map (kbd "SPC r s") #'wnh/shell)

    ;; Custom Keys
    (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

    ;(define-key evil-normal-state-map (kbd "C-S-h") #'windmove-swap-states-left)
    ;(define-key evil-normal-state-map (kbd "C-S-j") #'windmove-swap-states-down)
    ;(define-key evil-normal-state-map (kbd "C-S-k") #'windmove-swap-states-up)
    ;(define-key evil-normal-state-map (kbd "C-S-l") #'windmove-swap-states-right)

    (define-key evil-normal-state-map (kbd "C-S-h") #'tab-previous)
    (define-key evil-normal-state-map (kbd "C-S-j") #'tab-next)
    (define-key evil-normal-state-map (kbd "C-S-k") #'tab-previous)
    (define-key evil-normal-state-map (kbd "C-S-l") #'tab-next)

    (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

    (define-key evil-normal-state-map (kbd "SPC t n") #'tab-next)
    (define-key evil-normal-state-map (kbd "SPC t p") #'tab-previous)

    ;; Fat Fingers
    (evil-ex-define-cmd "W[rite]" #'evil-write)
    (evil-ex-define-cmd "Q[uit]" #'evil-quit)
    (evil-ex-define-cmd "Wq" #'evil-save-and-close)
    (evil-ex-define-cmd "WQ" #'evil-save-and-close)


    ;; org-mode
    (define-key evil-normal-state-map (kbd "SPC o i") #'org-clock-in)
    (define-key evil-normal-state-map (kbd "SPC o o") #'org-clock-out)


    ;; Taken for my vantedge work log - havent been timetracking that much anyway
    ;;(define-key evil-normal-state-map (kbd "SPC o l") #'org-clock-in-last)
    (define-key evil-normal-state-map (kbd "SPC o L") #'wnh/capture-log)
    (define-key evil-normal-state-map (kbd "SPC o c") #'org-capture)
    (define-key evil-normal-state-map (kbd "SPC o A") #'org-agenda)
    (define-key evil-normal-state-map (kbd "SPC o j") #'org-clock-goto)

    (define-key evil-normal-state-map (kbd "SPC o a") (lambda ()
							(interactive)
							(org-agenda nil "y")))

    (define-key evil-normal-state-map (kbd "SPC o w") #'wnh/working-mem-file)
    (define-key evil-normal-state-map (kbd "SPC o t") #'wnh/todo-file)

    (defun wnh/save-advice (&rest r)
      (save-buffer))

    (advice-add 'org-clock-in      :after #'wnh/save-advice)
    (advice-add 'org-clock-out     :after #'wnh/save-advice)
    (advice-add 'org-clock-in-last :after #'wnh/save-advice)

    (define-key evil-normal-state-map (kbd "C-S-b") #'switch-to-buffer)

    ;; error movement
    (define-key evil-normal-state-map (kbd "C-S-n") #'next-error)
    (define-key evil-normal-state-map (kbd "C-S-p") #'previous-error)

    (define-key evil-normal-state-map (kbd "C-S-m") #'wnh/run-q-macro))


(use-package undo-tree
  :ensure t
  :after evil
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1))

(defun wnh/ctrl-p ()
  "If the file is part of the gkroam directory, just use that to find
the file, otherwise find the file useing project.el"
  (interactive)
  (let ((fname (buffer-file-name (current-buffer))))
    (if (and fname
	     (string-match-p (concat "^" (expand-file-name "~") "/work/org/gkroam") fname))
      (gkroam-find)
    (project-find-file))))

(defun wnh/org-week-notes ()
  (interactive)
  (find-file "~/Dropbox/org/working-mem.org")
  (end-of-buffer))


(defun wnh/todo-file ()
  (interactive)
  (find-file "~/Dropbox/org/para.org"))

;;(defvar wnh/project-list '("~/work/src/API"
;;			   "~/work/src/Website"))
;;
;;(defun wnh/project-list-find (dir)
;;  (project-try-vc (completing-read "Project:" wnh/project-list)))
;;
;;(setq project-find-functions (append project-find-functions
;;				     (list #'wnh/project-list-find)))


(use-package project
  :config
  (define-key evil-normal-state-map (kbd "C-p") #'wnh/ctrl-p)
  (define-key evil-normal-state-map (kbd "SPC p p") #'project-switch-project)
  (define-key evil-normal-state-map (kbd "SPC p s") #'project-shell)
  (define-key evil-normal-state-map (kbd "SPC p e") #'project-eshell)
  (define-key evil-normal-state-map (kbd "SPC p d") #'project-dired)
  (define-key evil-normal-state-map (kbd "SPC p n") #'nodejs-repl)

  (setq project-switch-commands '((project-shell "Shell" ?s)
				  (project-find-file "Find file" ?p)
				  (project-find-regexp "Find regexp" ?f)
				  (project-find-dir "Find directory" ?d)
				  (magit "Magit" ?g))))

(use-package rg
  :ensure t
  :config

  (rg-define-search wnh/rg-project-js
    :query ask
    :files "py"
    :dir project)

  (define-key evil-normal-state-map (kbd "SPC s") #'rg-project)
  (define-key evil-normal-state-map (kbd "SPC f") #'wnh/rg-project-js))

(use-package magit
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "SPC g") #'magit-status)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package org
  :config
  (setq-default org-clock-report-include-clocking-task t)
  (setq org-adapt-indentation nil)
  (setq org-tags-column 0)
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)
  (setq org-tags-column 0)
  (setq org-todo-keywords
	'((sequence "TODO" "NEXT" "|" "DONE" "CANCELED")))
  (setq org-confirm-babel-evaluate nil)
  (setq wnh/org-para-file "~/Dropbox/org/para.org")
  (setq org-default-notes-file wnh/org-para-file)
  (setq org-agenda-files `(,wnh/org-para-file
			   "~/Dropbox/org/inbox_mobile.org"
			   "~/Dropbox/org/routines.org"
			   "~/Dropbox/org/brain.org"
			   "~/Dropbox/org/external.org"
			   "~/Dropbox/org/mind.org"
			   "~/Dropbox/org/proj.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline wnh/org-para-file "Inbox")
           "* TODO %?\n  %i\n  %a")
	  ("w" "Work" entry (file+headline "~/work/log.org" "Tasks")
           "* TODO %?\nCREATED: %u")
	  ("j" "Journal" entry (file+headline "~/Dropbox/org/journal.org" "New")
           "* %u %?")
	  ("L" "Work Log" entry (file+olp+datetree "~/Dropbox/org/WorkingJournal.org")
           "* %U  %?" :tree-type week)))

  (setq org-agenda-custom-commands
	'(("y" "Today + Next Actions"
	   ((agenda 1)
	    (todo "NEXT")))
	  ("i" "Custom test"
	   ((org-ql-block '(and (todo "TODO")
				(not (tags "recurring"))
				(not (effort))))))))

  (setq org-refile-targets
	`((wnh/org-para-file . (:tag . "Proj"))
	  (wnh/org-para-file . (:tag . "Area"))))

  ;; TODO (org-babel-do-load-languages
  ;; TODO   'org-babel-load-languages
  ;; TODO   '((shell . t)
  ;; TODO     ;;(mongo-dev . t)
  ;; TODO     (http . t)
  ;; TODO     ))

  (add-to-list 'org-export-backends 'md)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-default-notes-file "~/work/org/INBOX.org")
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "/Users/wharding/work/org/work.org" "TODOS")
           "* TODO %?\n%a\n%i"
	   :prepend t)))

  )

(use-package org-ql
  :ensure t)

;;
;; FONT STUFF
;;
(defvar wnh/font-is-big? nil
  "Have we bumped the font size the high DPI laptop screen?")
(set-face-attribute 'default nil :height wnh/font-small)
(defun wnh/toggle-font-size ()
  (interactive)
  (if wnh/font-is-big?
      (set-face-attribute 'default nil :height wnh/font-small)
    (set-face-attribute 'default nil :height wnh/font-large))
  (setq wnh/font-is-big? (not wnh/font-is-big?)))


(use-package eglot
  :ensure t
  :config
  (setq eldoc-idle-delay 5)
  (setq eglot-stay-out-of '(company))
  (add-to-list 'eglot-server-programs
	 '(js-mode "/Users/wharding/work/bin/js-lsp"))
  )

(defun wnh/project-get-filename ()
  "Use project-find-file to locate a file and then insert a relative
   path to that file"
  (let* ((project (project-current t))
         (dirs (list (project-root project)))
	 (suggested-filename (thing-at-point 'filename)))
    (let* ((all-files (project-files project dirs))
           (completion-ignore-case read-file-name-completion-ignore-case)
           (file (funcall project-read-file-name-function
                          "Find file" all-files nil nil
                          suggested-filename)))
      (if (string= file "")
          (user-error "You didn't specify the file")
	file))))

(defun wnh/js-insert-require ()
  (interactive)
  (let* ((fname (wnh/project-get-filename))
	 (rel-name (file-relative-name fname))
	 (with-dot-slash (if (not (string-prefix-p "." rel-name))
			     (concat "./" rel-name)
			   rel-name))
	 (module-name (replace-regexp-in-string "\.js$" "" with-dot-slash)))
    (insert (concat "require('" module-name "');"))))

(defun wnh/copy-location ()
  "grab the location at the pointer as 'path/to/filename:123' and push
it onto the kill ring"
  (interactive)
  (let ((full-name (buffer-file-name))
	(num (line-number-at-pos)))
    (kill-new (format "%s:%d" full-name num))))

(progn ;; JS config
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode nil)
	      (setq show-trailing-whitespace t)))
  (add-hook 'js-mode-hook #'flyspell-prog-mode)
  (evil-define-key 'normal js-mode-map
    (kbd "K") #'eldoc
    (kbd "SPC j r") #'wnh/js-insert-require
    (kbd "SPC h") #'wnh/copy-location))

(use-package clojure-mode
  :ensure t
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :ensure t
  :config
  (evil-define-key 'normal cider-repl-mode-map
    (kbd "SPC c") #'cider-repl-clear-buffer)
  (evil-define-key 'normal cider-mode-map
    (kbd "C-e") #'cider-eval-defun-at-point))

(use-package inf-clojure
  :ensure t
  :config
  (evil-define-key 'normal inf-clojure-minor-mode-map
    (kbd "C-e") #'inf-clojure-eval-defun))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))


(use-package restclient
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package olivetti
  :ensure t)

(defun wnh/async-shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (async-shell-command  (buffer-substring-no-properties start end)))

(define-key evil-normal-state-map (kbd "SPC q") #'indent-region)

(defun org-babel-execute:mongo-dev (body params)
  params)

(evil-define-key 'normal shell-mode-map
  (kbd "SPC c") #'comint-clear-buffer)

(defun initel ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package rust-mode
  :ensure t
  :after (eglot)
  :config
  ;; Use the rust-analyzer that rustup has installed
  (setf (alist-get 'rust-mode eglot-server-programs) '("wnh-rust-analyzer")))


(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode #'flyspell-mode)
  (add-hook 'markdown-mode #'auto-fill-mode))



(setq wnh/buffer-keepers '("*scratch*"))
(defun wnh/clean-buffers ()
  "Clean up the million buffer that you totally have

   Finds the buffers that 1. are not currently being displayed (ie:
   wont mess up the windows), 2. are not dirty (wont discard state),
   3. are not associated with an active process, and then kills them."
  ;; TODO: *Minibuf* and *Echo Area* buffers keep getting killed,
  ;;       doesn't seem to affect anything but those can probably be
  ;;       skipped
  (interactive)
  (let* ((visible-bufs (->> (window-list)
			   (-map #'window-buffer)))
	 (closeable (->> (buffer-list)
			 (-filter (lambda (buf)
				    (not
				     (or (and (buffer-file-name buf)
					      (buffer-modified-p buf))
					 (string= "*scratch*" (buffer-name buf))
					 (get-buffer-process buf)
					 (-contains? visible-bufs buf))))))))
    (dolist (buf closeable)
      (message "Cleaning: %s" buf)
      (kill-buffer buf))
    (message "Cleanded %d buffers" (length closeable))))


(comment ;; Testing to get *all* of the visibile buffers from *all* of the tabs
 (->> (tab-bar-tabs)
      (car)
      (assoc 'wc-bl)
      (length))

 (let ((x '()))
   (walk-windows (lambda (w) (setq x (cons w x))))
   x)

 (current-window-configuration))

(use-package dumb-jump
  ; :load-path "pkg-custom/dumb-jump"
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; TODO: Get SQL Working better: https://www.youtube.com/watch?v=D97vDu_BhwA
(use-package sql-indent
  :ensure t)
(add-hook 'sql-mode-hook
	  (lambda ()
	    (setq show-trailing-whitespace t)))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-expr-indent-offset 2)
  (setq typescript-indent-level 2))

;; has support support for jsx/tsx files
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package bigquery-mode
  :load-path "pkg-custom/bigquery-mode")

(use-package nodejs-repl
  :ensure t
  :config
  (evil-define-key 'normal js-mode-map (kbd "C-e") #'nodejs-repl-send-line)
  (evil-define-key 'visual js-mode-map (kbd "C-e") #'nodejs-repl-send-region))


;;(use-package textsize
;;  :ensure t
;;  :commands textsize-mode
;;  :init
;;  (setq textsize-default-points 12)
;;  (textsize-mode))


(defun wnh/wip-clean-buffers ()
  (let ((proc-buffers (mapcar #'process-buffer (process-list)))
	(visible-buffers (mapcar #'window-buffer (window-list)))
	(buffers (buffer-list)))
    (dolist (b buffers)
      (if (not (or (buffer-modified-p b)
		   (-contains? proc-buffers b)
		   (-contains? visible-buffers b)))
	  (message "should kill %s" (buffer-name b))
	  (message "SKIP %s" (buffer-name b))
	))))

;; (wnh/wip-clean-buffers)

;(use-package current-window-only
;  :load-path "lib"
;  :config
;  (current-window-only-mode 1))
;; (current-window-only-mode -1)

;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
;;(defadvice find-file (around find-file-line-number
;;                             (filename &optional wildcards)
;;                             activate)
;;  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
;;  (save-match-data
;;    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
;;           (line-number (and matched
;;                             (match-string 2 filename)
;;                             (string-to-number (match-string 2 filename))))
;;           (filename (if matched (match-string 1 filename) filename)))
;;      ad-do-it
;;      (when line-number
;;        ;; goto-line is for interactive use
;;        (goto-char (point-min))
;;        (forward-line (1- line-number))))))

(defun wnh/wip--select-prev-command ()
  ;; Run this in the comint buffer
  ;; (setq wnh/tmp comint-input-ring)

  (->> wnh/tmp
       (cddr)
       ;;(-map (lambda (x) (message "%s" x)))
       (-map #'substring-no-properties)
       (completing-read "Pick one: ")))



;; (load-file "~/tmp/xah-fly-keys.el")
;; (evil-mode -1)
;;
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty")
;;
;; (xah-fly-keys 1)

(use-package rcirc
  :config
  (setq rcirc-server-alist
	'(("irc.libera.chat"
	   :nick "wnh"
	   :username "wnh"
	   :channels ("#openbsd"))))
  (rcirc-track-minor-mode 1)
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (setq rcirc-authinfo '(("irc.libera.chat" nickserv "wnh" wnh/libera-chat-password)))
  )

(use-package add-node-modules-path :ensure t)
(use-package dash :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package go-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package nix-mode :ensure t)
(use-package ob-http  :ensure t)
(use-package olivetti :ensure t)
(use-package prettier-js :ensure t)
(use-package restclient :ensure t)
(use-package swift-mode :ensure t)
(use-package treemacs :ensure t)
(use-package yaml-mode :ensure t)

(use-package notmuch
  :ensure t
  :init
  (setq send-mail-function #'sendmail-send-it)
  (setq sendmail-program "gmail-send")
	
  (defun wnh/notmuch-delete ()
    (interactive)
    (notmuch-show-tag-all '("-inbox" "+trash"))
    (notmuch-show-next-thread-show))

  (defun wnh/notmuch-archive ()
    (interactive)
    (notmuch-show-tag-all '("-inbox" "-new"))
    (notmuch-show-next-thread-show))

  (defun wnh/gmail ()
    (interactive)
    (let ((buf (get-buffer-create "*gmail-sync*")))
      (async-shell-command "gmail-sync" buf)
      (with-current-buffer buf
	(evil-normal-state))))

  ;; :bind (:map notmuch-show-mode-map
  ;; 	      ("SPC d" . #'wnh/notmuch-delete))
  :config
  (evil-define-key 'normal notmuch-show-mode-map
    (kbd "SPC d")  #'wnh/notmuch-delete
    (kbd "SPC a")  #'wnh/notmuch-archive))

(use-package rc-mode
  :ensure t)



;;;; Open the shell for a project
;;(let ((default-directory "~/work/src/b/itd-1019/API"))
;;  (project-shell))
;;
;;;;  Not working - Try to spawn a new tab with certain name without
;;;;  switching to it
;;(let ((current-tab (tab-bar--current-tab-index)))
;;  (tab-new)
;;  (tab-rename "the new one")
;;  (tab-select current-tab))
;;


(defun wnh/launch-branch ()
  (interactive)
  (let* ((base  "~/work/src/b")
	 (branch-name
	  (completing-read 
	   "pick a dir: "
	   (let ()
	     (->> (directory-files base)
		  (-filter (lambda (f)
			     (file-directory-p (concat base "/" f))))))))
	 (branch-dir (concat base "/" branch-name))
	 (dirs '(("API" . "API")
		 ("Web" . "Website")
 		 ("Internal" . "th_internal_tools_api")
 		 ("Models" . "th_js_mongoModels"))))
    (dolist (d dirs)
      (tab-new)
      (tab-rename (car d))
      (let ((default-directory (concat branch-dir "/" (cdr d))))
	(project-shell)))))


;;;; Make it possible to copy email message IDs into notmuch links so that 
;;;;; from https://gist.github.com/fedxa/fac592424473f1b70ea489cc64e08911
(defun wnh/copy-email-id ()
    (interactive)
    (kill-new
     (concat "[[notmuch:" (notmuch-show-get-message-id) "]"
	     "[" (notmuch-show-get-subject) "]]")))

(org-link-set-parameters "notmuch"
			 :follow #'org-notmuch-open
			 ;; :store 'org-notmuch-store-link
			 )

(defun org-notmuch-open (id)
  "Visit the notmuch message or thread with id ID."
  (notmuch-show id))

(defun org-notmuch-store-link ()
  "Store a link to a notmuch mail message."
  (case major-mode
    ('notmuch-show-mode
     ;; Store link to the current message
     (let* ((id (notmuch-show-get-message-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-show-get-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))
    ('notmuch-search-mode
     ;; Store link to the thread on the current line
     (let* ((id (notmuch-search-find-thread-id))
	    (link (concat "notmuch:" id))
	    (description (format "Mail: %s" (notmuch-search-find-subject))))
       (org-store-link-props
	:type "notmuch"
	:link link
	:description description)))))


(defun wnh/open-journal-today ()
  (interactive)
  (let* ((file-name (concat "~/Dropbox (Maestral)/journal/"
			    (format-time-string "%Y-%m-%d")
			    ".txt"))
	 (header (format-time-string "%a %d %b %Y"))
	 (buf (find-file file-name)))
    (with-current-buffer buf
      (if (file-exists-p file-name)
	  (set-window-point (get-buffer-window buf) (point-max))
	(insert header "\n\n")))))

(define-key evil-normal-state-map (kbd "SPC w j") #'wnh/open-journal-today)

(use-package scheme-mode
  :bind (:map scheme-mode-map
	      ("C-e" . xscheme-send-definition)))

(setq notmuch-search-oldest-first nil)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(("http://nullprogram.com/feed/" software)
          ("https://planet.emacslife.com/atom.xml" software emacs)
	  ("https://www.robinsloan.com/feed.xml" books software)
	  ("https://joshondesign.com/feed" software design))))

(use-package notmuch
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package tuareg
  :ensure t)
(use-package reason-mode
  :ensure t)
