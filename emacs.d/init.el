;; The manual - https://www.gnu.org/software/emacs/manual/pdf/eintr.pdf

;;  TODO - "cw" to change-word doesn't this to work correctly in some contexts
;;         Maybe it was when there was a search active in the minibuffer? 
;;         It seems to be working now?
;; TODO figure out the per-file-type tab/expand stuff for emacs
;; TODO make redo work
;; TODO map "W" to "w"
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(require 'package)


(setq package-check-signature nil)

(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
                         ;("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(defvar wnh/packages
  '(ag
    cider
    clojure-mode
    inf-clojure
    markdown-mode
    evil-collection
    magit
    neotree ;;  https://github.com/jaypei/emacs-neotree
    solarized-theme
    spacemacs-theme

    ;; only keep one of these
    projectile
    ivy
    ;;councel
    ))

(setq gc-cons-threshold 20000000)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package ivy
  :ensure t)
(use-package counsel
  :ensure t)

(use-package geiser :disabled)
(use-package geiser-chibi :disabled)

(use-package ag
  :ensure t)


(defvar wnh/todo-file "~/org/TODO.org")

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs" ((agenda "")
				     (alltodo "")))
	("x" "Two days with TODOs" ((agenda "" ((org-agenda-span 2)))
				    (alltodo "")))))

;; To invoke a custom command from elisp: (org-agenda nil "x")

(defun wnh/org-agenda-done ()
  (interactive)
  (org-agenda-todo "DONE"))


(use-package org
  :init
    ;; Stop org mode from hard aligning (with acutal spaces) text to match
    ;; the heading indentation.
    ;; Look (without extra characters) can be achived with org-indent-mode
    (setq org-adapt-indentation nil)
    (setq org-default-notes-file wnh/todo-file)
    (setq org-agenda-files `(,wnh/todo-file
			     "~/work/org/gkroam/"))
    (setq org-capture-templates
	  `(("t" "Todo" entry (file+headline ,wnh/todo-file "Inbox")
	     "* TODO %?\nCreated: %U\n%i")
	    ("j" "Journal" entry (file+datetree "~/org/journal.org")
	     "* %U\n%?")))

    ;; Go into insert mode when I capture something
    (add-hook 'org-capture-mode-hook 'evil-insert-state)
    
    (add-hook 'org-agenda-mode-hook (lambda ()
				      (local-set-key (kbd "C-d") 'wnh/org-agenda-done)))
			    

    ;; This makes the tags flush against the heading not floating out
    ;; into space. A lot nicer for other editors looking at the file
    (setq org-tags-column 0)

    (setq org-refile-targets
  	  `((,wnh/todo-file :maxlevel . 1)))

    (setq org-log-done 'time)

    ;;bind ("C-c c" . org-capture)

    ;; TODO: Why is this breaking now?
    ;;:config
    ;;  (org-babel-do-load-languages
    ;;   'org-babel-load-languages
    ;;   '((shell  . t)
    ;;     (python . t)
    ;;     (sql    . t))))
    )

(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell  . t)
			       (python . t)
			       (sql    . t)))

(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package evil
  :ensure t
  :init
    (setq evil-want-keybinding nil)
  :config
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  
    (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
    ;; Still not sure why this isnt here?
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

    ;; this doesn't seem to be working
    (add-hook 'term-mode-hook
	      (lambda ()
		(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
		(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
		(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
		(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))
    
    (evil-define-key 'normal org-mode-map "gq" 'org-fill-paragraph)

    (define-key evil-normal-state-map (kbd "C-p")   'projectile-find-file)

    ;; Deal with my fat fingers
    ;;  - evil-maps.el for all the good examples
    (evil-ex-define-cmd "W[rite]" 'evil-write)
    (evil-ex-define-cmd "Q[uit]"  'evil-quit)
    (evil-ex-define-cmd "Wq" 'evil-save-and-close)
    (evil-ex-define-cmd "WQ" 'evil-save-and-close)
    (setq evil-lookup-function #'man))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))



;; TODO learn how to do the IDO things (still need a good ctrl-p alternative
;;  - I'm not sure if these two top lines 
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)

(use-package emacs
  :config
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (global-display-line-numbers-mode)

    (global-set-key (kbd "C-S-b") 'ivy-switch-buffer)

    (add-hook 'emacs-lisp-mode-hook
	      (lambda () (define-key evil-normal-state-local-map
			   (kbd "C-e") 'eval-defun)))

    ;; Embiggen the font
    '(set-face-attribute 'default nil :height 110)

    )



(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
	    (lambda () (define-key evil-normal-state-local-map
			 (kbd "C-e") 'cider-eval-defun-at-point))))

;; You can use eval defun inside a comment block and it will grab the
;; top-most thing inside the comment. Kinda what you want to happen
(setq clojure-toplevel-inside-comment-form t)
(setq cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "TAB") 'markdown-cycle)
	      (define-key evil-normal-state-local-map (kbd "S-TAB") 'markdown-shifttab))))

(defun wnh/open-todo ()
  (interactive)
  (find-file-other-window wnh/todo-file))


;; TODO: where should this go??j
(defun wnh/org-insert-daily ()
  (interactive)
  (insert "* Daily ")
  (insert (format-time-string "[%Y-%m-%d %a]"))
  (insert "\n** Standup\n** Review PRs?\n** Mail/Slack/Admin\n\n"))

(defun wnh/clock-in ()
  (interactive)
  (org-clock-in)
  (save-buffer))

(defun wnh/clock-out ()
  (interactive)
  (org-clock-out)
  (save-buffer))

(defun wnh/clock-in-last ()
  (interactive)
  (org-clock-in-last)
  (save-buffer))

(defun wnh/schedule-today ()
  (interactive)
  (org-schedule nil "."))

(defun wnh/schedule-tomorrow ()
  (interactive)
  (org-schedule nil "+1d"))


(define-key evil-normal-state-map (kbd "SPC o t") #'wnh/schedule-today)
(define-key evil-normal-state-map (kbd "SPC o T") #'wnh/schedule-tomorrow)
(define-key evil-normal-state-map (kbd "SPC o i") #'wnh/clock-in)
(define-key evil-normal-state-map (kbd "SPC o o") #'wnh/clock-out)
(define-key evil-normal-state-map (kbd "SPC o l") #'wnh/clock-in-last)
(define-key evil-normal-state-map (kbd "SPC o f") #'deft)
; Used for gkroam-insert with [N]ode
;(define-key evil-normal-state-map (kbd "SPC o n") #'wnh/org-insert-daily)
(define-key evil-normal-state-map (kbd "SPC o c") #'org-capture)
(define-key evil-normal-state-map (kbd "SPC o A") #'org-agenda)
(define-key evil-normal-state-map (kbd "SPC o a") (lambda ()
						    (interactive)
						    (org-agenda-list 2)))

(defun wnh/open-initel ()
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

(evil-ex-define-cmd "init" 'wnh/open-initel)
(evil-ex-define-cmd "todo" 'wnh/open-todo)



;; TODO how the heck to I make this work with use-package?
(load-theme 'spacemacs-light)
;; Allows you to cycle back through window configs 
;; learn more about this
(winner-mode +1)


;; Show those parens
(setq show-paren-delay 0)
(show-paren-mode +1)

(setq projectile-completion-system   'ivy)
;(projectile-add-known-project "~/oss/guile")
(projectile-add-known-project "~/work/src/API")
(projectile-add-known-project "~/work/src/Website")
(projectile-add-known-project "~/work/src/tools_api")

;; TODO - Get Fuzz finding working
;;    https://github.com/silentbicycle/ff
;;    https://melpa.org/#/helm-fuzzy-find



(add-to-list 'exec-path "/home/wharding/bin")
(add-to-list 'exec-path "/home/wharding/local/bin")
(add-to-list 'exec-path "/home/wharding/local/Apps/bin")
(add-to-list 'exec-path "/home/wharding/.nvm/versions/node/v13.12.0/bin")

(hl-line-mode 1)

;; put all backup/autosave files into the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Custom time tracking tables for emacs
;; maybe something better than this? https://www.emacswiki.org/emacs/LoadingLispFiles
(load "~/.emacs.d/wnh-org-custom")



(defun wnh/rg ()
  (interactive)
  (let ((root (projectile-project-root))
	(search "Scheduled"))
    (compilation-start (format "rg %s %s" search root))))
    ;(message "Arg: %s" s)))


;;; deal with long lines
;;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(setq-default bidi-paragraph-direction 'left-to-right)
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))


(defun wnh/text-scale-all (n)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (text-scale-adjust n))))

(setq-default line-spacing 0.4)


(defun wnh/writing ()
  (interactive)
  (let ((date (format-time-string "%Y-%m-%d.txt")))
    (find-file (concat "~/wrt/" date ".txt"))
    (display-line-numbers-mode -1)
    (flyspell-mode)
    (olivetti-mode)))

(defun wnh/show-me-ticket (ticket-id)
  "Should we set this as readonly after? maybe notes are a good thing
   to have"
  (interactive)
  (with-current-buffer (get-buffer-create "*jira*")
    (erase-buffer)
    ;; also: set cursor to the top
    ;; pop the buffer up
    (insert (shell-command-to-string (concat "jira " ticket-id)))))

(defun wnh/show-me-jira ()
  "Should we set this as readonly after? maybe notes are a good thing
   to have"
  (interactive)
  (with-current-buffer (get-buffer-create "*jira*")
    (erase-buffer)
    ;; also: set cursor to the top
    ;; pop the buffer up
    (insert (shell-command-to-string "jira"))))

(evil-ex-define-cmd "jira" 'wnh/show-me-jira)


; stop the .#.<filename> things that seem to break a bunch of file
; watchers
(setq create-lockfiles nil)




(defun wnh/journal-today ()
  (interactive)
  (with-current-buffer (find-file-other-window "~/org/journal.org")
    (widen)
    (org-datetree-find-date-create (calendar-current-date))
   ;; (org-narrow-to-subtree)
    ))


(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-autojoin-channels-alist
        '(("irc.libera.chat" "#lobsters" "#openbsd" "#clojure")))
  (defun wnh/libera ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
	     :port 6697
             :nick "wnh"
	     :full-name "wnh")))

(use-package elfeed
  :ensure t
  :config
  (setq shr-width 80)
  (setq elfeed-feeds
	'(("https://rakhesh.com/feed/")
	  ("https://nullprogram.com/feed/")
	  ("https://rachelbythebay.com/w/atom.xml")
	  ("https://blog.jse.li/index.xml")
	  ("https://m.signalvnoise.com/feed/" dev)
	  ("https://buttondown.email/nelhage/rss")
	  ("https://soatok.blog/feed/" crypto)
	  ("https://undeadly.org/cgi?action=rss&items=10&full=true" openbsd)
	  ("https://minimalmodeling.substack.com/feed")
	  ("https://jcs.org/rss" openbsd)
	  ("https://dave.cheney.net/feed/atom")
	  ("https://tailscale.com/blog/index.xml" golang)
	  ("http://planet.clojure.in/atom.xml" clojure)
	  ("https://christine.website/blog.rss")
	  ("https://quanttype.net/index.xml")
	  ("https://ftrv.se/posts.rss")
	  ("https://flak.tedunangst.com/rss" openbsd)
	  ("https://wiki.xxiivv.com/links/rss.xml")
	  ("https://www.hillelwayne.com/post/index.xml")
	  ("https://seh.dev/index.xml")
	  ("https://computer.rip/rss.xml")
	  ("https://ferd.ca/feed.rss" systems)
	  ("https://seh.dev/index.xml" plan9)
	  ("https://briancallahan.net/blog/feed.xml" openbsd))))


;; fonts
(defvar wnh/font-big nil
  "Have we bumped the font size the high DPI laptop screen")

(set-face-attribute 'default nil :height 90)
(defun wnh/toggle-font-size ()
  (interactive)
  (let ((big    110)
	(small   90)) 
    (if wnh/font-big
	(set-face-attribute 'default nil :height small)
      (set-face-attribute 'default nil :height big))
    (setq wnh/font-big (not wnh/font-big))))


;; font size stuff
; TODO(wnh): is this causing incorrect colors for :logbook: drawers in
; org-mode
; For  work monitor
; (set-frame-font "DejaVuSansMono-9" t nil)
; For the mobile screen
;(set-frame-font "DejaVuSansMono-11" t nil)

;;; roam
(use-package gkroam
  :ensure t
  :hook (after-init . gkroam-mode)
  :init
  (setq gkroam-root-dir "~/work/org/gkroam")
  (setq gkroam-prettify-page-p t
        gkroam-show-brackets-p nil
        gkroam-use-default-filename t
        gkroam-window-margin 4)
  (define-key evil-normal-state-map (kbd "SPC o d") #'gkroam-daily)
  (define-key evil-insert-state-map (kbd "C-l") #'gkroam-insert))

(use-package deft
  :ensure t
  :init
  (setq deft-extensions '("txt" "org"))
  (setq deft-directory "~/work/org/gkroam"))

(use-package neotree
  :ensure t)

;; JAVASCRIPT
;; 
(setq js-indent-level 2)

(use-package prettier-js
  ;; :ensure t

  :config
  ;; TODO - This might be handy
  ;; https://github.com/codesuki/add-node-modules-path
  (add-to-list 'exec-path "/home/wharding/.nvm/versions/node/v13.12.0/bin")
  ;; turn prettier on when js is started
  ;; maybe I should limit this to only the work projects? 
  ;; (add-hook 'js-mode-hook 'prettier-mode)
  )

(use-package flycheck
  :ensure t)

(defun wnh/path-to-dom-file (src fname)
  (let ((config-dir (locate-dominating-file src fname)))
    (if config-dir
	(concat config-dir fname))))


(defun wnh/js-run-eslint-compile ()
  (interactive)
  (let* ((src (buffer-file-name (current-buffer)))
	 (config-file (or (wnh/path-to-dom-file src ".eslintrc.json")
			  (wnh/path-to-dom-file src ".eslintrc")))
	 (config-dir (file-name-directory config-file))
	 (lint (concat config-dir  "node_modules/.bin/eslint"))
	 )
    (compile (string-join (list lint "--format" "unix" src) " "))))

(defun wnh/js-run-eslint-compile ()
  (interactive)
  (let* ((src (buffer-file-name (current-buffer)))
	 (default-directory (locate-dominating-file src ".git"))
	 (xxx (message default-directory))
	 ;(npx "/home/wharding/.nvm/versions/node/v13.12.0/bin/npx")
	 (lint (concat default-directory "node_modules/.bin/eslint"))
	 )
    (compile (string-join (list lint "eslint" "--format" "unix"  src) " "))))

(defun wnh/js-setup-hook ()
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (define-key evil-normal-state-map (kbd "C-S-n") #'next-error)
  (define-key evil-normal-state-map (kbd "C-S-p") #'previous-error)
  (define-key evil-normal-state-map (kbd "SPC e e") #'wnh/js-run-eslint-compile))

(add-hook 'js-mode-hook #'wnh/js-setup-hook)

(use-package rg
  :ensure t)
(setq org-clock-report-include-clocking-task t)
(evil-ex-define-cmd "r" 'projectile-ripgrep)
(evil-set-initial-state 'deft-mode 'emacs)

(defun deft-current-window-width ()
  (- (window-body-width (get-buffer-window deft-buffer)) 5))

(use-package deft
  :ensure t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
