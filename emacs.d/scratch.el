

(defun wnh/minutes-to-time (mins)
  (format "%d:%02d" (/ mins 60) (mod mins 60)))

(let* ((time (decode-time))
       (day (nth  3 time)) ;; day
       (month (nth  4 time)))
  (message "%d-%d" month day)) ;; month

nth

(wnh/to-time 1081)
(wnh/to-time 347)

cider-cljs-repl-types

(with-temp-buffer
  (insert-file-contents "~/TODO.org")
  (org-mode)
  (org-clock-get-table-data "todo.org"
			    '()))

(decode-time)

(let* ((day 17);(nth 4 (decode-time)))
       (mth (nth 3 (decode-time)))
       (first? (and (<= 1 day)
                    (>= 15 day))))
  (list first?))

(current-time-string)
(decode-time (current-time))

(date-to-time "2020-09-01")
(time-to-date (current-time))



