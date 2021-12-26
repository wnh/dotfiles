;; Extra functions make them go better places
(defun wnh/server-run-compile ()
  (interactive)
  ;; Is this dynamic binding in action? not sure?
  (let ((default-directory (projectile-project-root)))
    (compile "/home/wharding/local/Apps/apache-maven-3.6.3/bin/mvn -B compile")))

(defun wnh/sever-run-deploy ()
  (interactive)
  ;; Is this dynamic binding in action? not sure?
  (let ((default-directory (projectile-project-root)))
    (compile
      "PATH=$PATH:/home/wharding/local/Apps/apache-maven-3.6.3/bin sh ./deploy.sh")))

(defun wnh/load-server ()
  (interactive)
  (define-key evil-normal-state-map (kbd "SPC r d") 'wnh/server-run-deploy)
  (define-key evil-normal-state-map (kbd "SPC r c") 'wnh/server-run-compile))
