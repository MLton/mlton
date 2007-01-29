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
       (def-use-intern (file-truename file))))
    def-use-intern-table))

(defun def-use-buffer-true-file-name ()
  "Returns the true filename of the current buffer."
  (def-use-file-truename (buffer-file-name)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-util)
