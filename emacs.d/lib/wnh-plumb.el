
(defun wnh/plumb-at-point ()
  (interactive)
  (message "%s" "I think it worked"))

(define-minor-mode wnh/plumb-mode
  "Get your foos in the right places."
  :lighter " Plmb"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map <mouse-3> #'wnh/plumb-at-point)
            map))

