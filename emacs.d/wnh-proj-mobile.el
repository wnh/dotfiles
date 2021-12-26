
(defun wnh/mobile-run-trans ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    ;(compile "pwd")
    (compile "sh ./scan-translations.sh")))

(defvar wnh/mobile-server-proc nil)

(defun wnh/mobile-run-server ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (when wnh/mobile-server-proc
      (message "%s" "Stopping existing server...")
      (wnh/mobile-kill-server))
    (message "%s" "Starting server...")
    (setq wnh/mobile-server-proc
	  (start-process "mobile-server"
			 "*mobile-server*"
			 "/home/wharding/.nvm/versions/node/v10.16.3/bin/npm"
			 "start"))))
  
(defun wnh/mobile-kill-server ()
  (interactive)
  (interrupt-process wnh/mobile-server-proc))

wnh/mobile-server-proc2

(mapcar  (lambda (p) (list (process-status p)
			   (process-name p)))
	 (process-list))

(process-buffer wnh/mobile-server-proc)

(dolist (p (process-list))
  (interrupt-process p))

(defun wnh/load-mobile ()
  (interactive)
  ;; Stop the lock files from breaking
  (setq create-lockfiles nil)
  (define-key evil-normal-state-map (kbd "SPC r t") 'wnh/mobile-run-trans)
  (define-key evil-normal-state-map (kbd "SPC r s") 'wnh/mobile-run-server))
