(setq exec-path
      (cons "/Users/wharding/.nix-profile/bin" exec-path))

(setq ring-bell-function 'ignore)
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

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
    (cond
     ((string= (system-name) "frmwrk")
      (setq wnh/font-small 85)
      (setq wnh/font-large 110)
      (menu-bar-mode -1)
      (set-face-attribute 'default nil :family "DejaVu Sans")
      (setq-default line-spacing 0.6))
     ((string= (system-name) "x1")
      (setq wnh/font-small 90)
      (setq wnh/font-large 110)
      (menu-bar-mode -1)
      (setq-default line-spacing 0.4))
     ((string= system-type  "darwin")   ;"Wills-MBP.localdomain"
      (setq mac-command-modifier 'meta)
      (global-display-line-numbers-mode -1)
      ;(set-face-attribute 'default nil :family "Fira Sans")
      ;(set-face-attribute 'default nil :family "~/.nix-profile/share/fonts/truetype/iosevka-regular.ttf")
      (set-face-attribute 'default nil :family "Iosevka Term")
      (set-face-attribute 'default nil :height 80)
      (set-face-attribute 'default nil :height 80)
      (set-face-attribute 'tab-bar nil :height 160)
      ;(set-face-attribute 'default nil :family "Verdana")
      ;(set-face-attribute 'default nil :height 110)
      (setq wnh/font-small 110)
      (setq wnh/font-large 130)
      (setq-default line-spacing 0.8)))

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


(use-package evil
  :ensure t
  :init
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
  :config
    (evil-mode 1)

    (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-e") #'eval-defun)

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
    (define-key evil-normal-state-map (kbd "SPC o l") #'org-clock-in-last)
    (define-key evil-normal-state-map (kbd "SPC o j") #'org-clock-goto)
    (define-key evil-normal-state-map (kbd "SPC o a") (lambda ()
							(interactive)
							(org-agenda-list 2)))

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


(use-package spacemacs-theme
  :disabled t
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold t)
  (load-theme 'solarized-light))


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

(defun wnh/working-mem-file ()
  (interactive)
  (gkroam-find "WorkingMem")
  (end-of-buffer))

(defun wnh/todo-file ()
  (interactive)
  (gkroam-find "todo"))

(use-package gkroam
  :ensure t
  :hook (after-init . gkroam-mode)
  :init
    (setq gkroam-root-dir "~/work/org/gkroam"
	  gkroam-prettyify-page-p t
	  gkroam-show-brackets-p t
	  gkroam-use-default-filename t
	  gkroam-window-margin 4)
    (define-key evil-normal-state-map (kbd "SPC o d") #'gkroam-daily)
    (define-key evil-normal-state-map (kbd "SPC o w") #'wnh/working-mem-file)
    (define-key evil-normal-state-map (kbd "SPC o t") #'wnh/todo-file)
    (define-key evil-normal-state-map (kbd "SPC o f") #'gkroam-find)

    (define-key evil-normal-state-map (kbd "SPC o t")
      (lambda ()
	(interactive)
	(gkroam-find "todo")))

    (define-key evil-insert-state-map (kbd "C-S-l") #'gkroam-insert)
    :config
    (evil-define-key 'insert gkroam-mode-map
      (kbd "C-S-l") #'gkroam-insert)

    ;;(add-hook 'gkroam-mode-hook
    ;;          (lambda ()
    ;;		(evil-local-set-key 'normal (kbd "C-p") #'gkroam-find)))

    (add-hook 'gkroam-mode-hook 'flyspell-mode))

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
    :files "js"
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
	'((sequence "TODO" "|" "DONE" "CANCELED")))
  (setq org-agenda-files `("~/work/org"
			   "~/Dropbox/org"))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((shell . t)
      ;;(mongo-dev . t)
      (http . t)
      ))

  (add-to-list 'org-export-backends 'md)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-default-notes-file "~/work/org/INBOX.org"))

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



(use-package popper
  :ensure t
  :bind (("C-\\" . popper-toggle-latest)
	 ("C-|" . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          "\\*xref\\*"
          ;;"\\*rg\\*" ;; this is kinda weird now
          help-mode
          ;;compilation-mode
	  ;;shell-mode
	  ))
  (popper-mode +1)
  (popper-echo-mode +1))

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


(defun wnh/async-shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (async-shell-command  (buffer-substring-no-properties start end)))

(define-key evil-normal-state-map (kbd "SPC q") #'indent-region)

(defun org-babel-execute:mongo-dev (body params)
  (stringp body))

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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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
  :load-path "pkg-custom/dumb-jump"
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


(use-package textsize
  :ensure t
  :commands textsize-mode
  :init
  (setq textsize-default-points 12)
  (textsize-mode))


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
  (setq rcirc-server-alist '(("irc.libera.chat"
			      :nick "wnh"
			      :username "wnh"
			      :channels ("#openbsd"))))
  ;;(setq rcirc-auth-info '(()))
  ;; /msg NickServ IDENTIFY wnh <password>
  )

(defun wnh/jira-go ()
  (interactive)
  (let ((id (thing-at-point 'symbol 'no-properties)))
    (if (string-match (rx (: (or "ITD" "BIN" "PRM" "API") "-" (+ digit))) id)
	(browse-url (concat "https://taskhuman.atlassian.net/browse/" id))
      (message "Not a valid ticket: %s" id))))
(define-key evil-normal-state-map (kbd "g t") #'wnh/jira-go)

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
