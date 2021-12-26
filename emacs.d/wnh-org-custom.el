
(require 'org)
(require 'org-clock)

(defun wnh/day-letter (time)
  (let* ((date-string (current-time-string time))
         (day-name (car (split-string date-string))))
    (substring day-name 0 1)))

;;(wnh/day-letter (current-time))

(defun wnh/minutes-to-time (mins)
  (format "%d:%02d" (/ mins 60) (mod mins 60)))

(defun wnh/time-to-date (time)
  (let ((d (decode-time time)))
    (format "%04d-%02d-%02d"
            (nth 5 d)
            (nth 4 d)
            (nth 3 d))))

(defun wnh/back-n-days (n)
  (let ((dates nil))
    (dotimes (i n)
      (let* ((day-secs (* 24 60 60))
             (now (current-time))
             (days-back (* -1 i day-secs))
             (time-back (time-add now days-back)))
        (add-to-list 'dates
                     time-back)))
    dates))

;; (wnh/back-n-days 4)

(defun wnh/time-is-sun-p (time)
  (let* ((tt (decode-time time))
         (dow (nth 6 tt)))
    (or ;;(= dow 5)
        (= dow 0))))


(defun wnh/is-first-pay (date)
  (<= (nth 3 (decode-time date)) 15))


(defun wnh/last-day-of-month (date)
  (let* ((day-secs (* 24 60 60))
         (tomorrow (time-add date day-secs)))
    (= (nth 3 (decode-time tomorrow)) 1)))

(defun wnh/is-first-pay-break (date)
  (let ((d (nth 3 (decode-time date))))
    (or (= d 15)
        (wnh/last-day-of-month date))))

(defvar wnh/timesheet-days 15)

(defun org-dblock-write:wnh/timesheet (params)
  "this is all that is required for 
#+BEGIN: wnh/timesheet
to work in org-mode"
  (let ((days (plist-get params :days)))
    (dolist (date (wnh/back-n-days (if days days wnh/timesheet-days)))
      (let* ((date-str (wnh/time-to-date date))
             (clock-data (org-clock-get-table-data
                          "todo.org"
                          `(:scope file :block ,date-str :maxlevel 2)))
             (day-time (car (cdr clock-data))))

        (insert (format "%s %s %5s%s%s\n"
                        date-str
                        (wnh/day-letter date)
                        (if (eq 0 day-time)
                            "-"
                          (wnh/minutes-to-time day-time))
                        (if (wnh/time-is-sun-p date)
                            "\n"
                          " ")
                        (if (wnh/is-first-pay-break date) "\n------------\n" " ")
                        ))))))
