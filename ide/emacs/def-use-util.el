;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defvar def-use-file-truename-table
  (make-hash-table :test 'equal :weakness 'key)
  "Weak hash table private to `def-use-file-truename'.")

(defun def-use-file-truename (file)
  "Cached version of `file-truename'."
  (def-use-gethash-or-put file
    (function
     (lambda ()
       (def-use-intern
         (def-use-add-face 'change-log-file-face
           (file-truename file)))))
    def-use-file-truename-table))

(defun def-use-buffer-true-file-name ()
  "Returns the true filename of the current buffer."
  (let ((name (buffer-file-name)))
    (when name
      (def-use-file-truename name))))

(defun def-use-point-at-next-line ()
  "Returns point at the beginning of the next line."
  (save-excursion
    (end-of-line)
    (+ 1 (point))))

(defun def-use-point-at-current-line ()
  "Returns point at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun def-use-delete-idle-timer (timer)
  "Deletes the specified idle timer."
  (if (string-match "XEmacs" emacs-version)
      (delete-itimer timer)
    (cancel-timer timer)))

(defun def-use-gethash-or-put (key_ mk-value_ table_)
  (or (gethash key_ table_)
      (puthash key_ (funcall mk-value_) table_)))

(defvar def-use-intern-table
  (make-hash-table :test 'equal :weakness 'key-and-value)
  "Weak hash table private to `def-use-intern'.")

(defun def-use-intern (value)
  "Hashes the given value to itself.  The assumption is that the value
being interned is not going to be mutated."
  (def-use-gethash-or-put value (function (lambda () value))
    def-use-intern-table))

(defun def-use-hash-table-to-assoc-list (hash-table)
  "Returns an assoc list containing all the keys and values of the hash
table."
  (let ((result nil))
    (maphash (function
              (lambda (key value)
                (push (cons key value) result)))
             hash-table)
    (nreverse result)))

(defun def-use-hash-table-to-key-list (hash-table)
  "Returns a list of the keys of the set (identity hash-table)."
  (mapcar (function car)
          (def-use-hash-table-to-assoc-list hash-table)))

(defun def-use-set-to-list (set)
  "Returns a list of the keys of the set (identity hash-table)."
  (def-use-hash-table-to-key-list set))

(defun def-use-make-hash-table ()
  "Makes a hash table with `equal' semantics."
  (make-hash-table :test 'equal :size 1))

(defun def-use-kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun def-use-add-face (face string)
  "Adds the face as a property to the entire string and returns the
string."
  (add-text-properties 0 (length string) `(face ,face) string)
  string)

(defun def-use-time-to-double (time)
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (caddr time) 1000000.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background Processor

(defun def-use-bg-job (done? step finalize args)
  (cons args (cons done? (cons step finalize))))
(defun def-use-bg-job-done? (job)
  (apply (cadr job) (car job)))
(defun def-use-bg-job-step (job)
  (setcar job (apply (caddr job) (car job))))
(defun def-use-bg-job-finalize (job)
  (apply (cdddr job) (car job)))

(defvar def-use-bg-jobs nil)

(defconst def-use-bg-job-period 0.03)
(defconst def-use-bg-job-cpu-ratio 0.7)

(defun def-use-bg-job-reschedule ()
  (when def-use-bg-jobs
    (run-with-timer
     (- (/ def-use-bg-job-period def-use-bg-job-cpu-ratio)
        def-use-bg-job-period)
     nil
     (function def-use-bg-job-quantum))))

(defun def-use-start-bg-job (done? step finalize &rest args)
  (let ((schedule (not def-use-bg-jobs)))
    (push (def-use-bg-job done? step finalize args) def-use-bg-jobs)
    (when schedule
      (def-use-bg-job-reschedule))))

(defun def-use-bg-job-quantum ()
  (let ((start-time (def-use-time-to-double (current-time))))
    (while (and def-use-bg-jobs
                (< (- (def-use-time-to-double (current-time))
                      start-time)
                   def-use-bg-job-period))
      (let ((job (pop def-use-bg-jobs)))
        (if (def-use-bg-job-done? job)
            (def-use-bg-job-finalize job)
          (def-use-bg-job-step job)
          (setq def-use-bg-jobs
                (nconc def-use-bg-jobs (list job)))))))
  (def-use-bg-job-reschedule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-util)
