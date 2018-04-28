;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'cl)
(require 'compat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; In Gnu Emacs, `buffer-file-truename' is abbreviated while in XEmacs it
;; isn't.  This isn't in compat.el, because we want to use our cached
;; version of `file-truename', namely `def-use-file-truename'.
(defun def-use-buffer-file-truename (&rest buffer)
  "Returns the true filename of the current buffer."
  (let ((name (apply (function buffer-file-name) buffer)))
    (when name
      (def-use-file-truename name))))

(defvar def-use-file-truename-table
  (make-hash-table :test 'equal :weakness 'key)
  "Weak hash table private to `def-use-file-truename'.")

(defun def-use-file-truename (file)
  "Cached version of `file-truename' combined with `abbreviate-file-name'."
  (def-use-gethash-or-put file
    (function
     (lambda ()
       (def-use-intern
         (def-use-add-face 'font-lock-keyword-face
           (compat-abbreviate-file-name (file-truename file))))))
    def-use-file-truename-table))

(defun def-use-find-buffer-visiting-file (file)
  "Tries to find a buffer visiting the specified file."
  (let ((truename (def-use-file-truename file)))
    (loop for buffer in (buffer-list) do
      (if (with-current-buffer buffer
            (string= (def-use-buffer-file-truename) truename))
          (return buffer)))))

(defun def-use-find-file (file &optional other-window)
  "Roughly as `find-file' or `find-file-other-window' except that will not
open the file a second time if a buffer is editing a file by the same true
file name."
  (let ((buffer (def-use-find-buffer-visiting-file file)))
    (cond
     (buffer
      (let ((window (get-buffer-window buffer)))
        (cond
         (other-window
          (switch-to-buffer-other-window buffer))
         (window
          (set-frame-selected-window nil window))
         (t
          (switch-to-buffer buffer)))))
     (other-window
      (find-file-other-window file))
     (t
      (find-file file)))))

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

(defun def-use-current-line ()
  "Returns the current line number counting from 1."
  (+ 1 (count-lines 1 (def-use-point-at-current-line))))

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
    result))

(defun def-use-hash-table-to-key-list (hash-table)
  "Returns a list of the keys of hash-table."
  (mapcar (function car)
          (def-use-hash-table-to-assoc-list hash-table)))

(defun def-use-hash-table-to-value-list (hash-table)
  "Returns a list of the values of the hash-table."
  (mapcar (function cdr)
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
  "Converts a time to a double."
  (+ (* (car time) 65536.0)
     (cadr time)
     (if (cddr time) (* (caddr time) 1e-06) 0)))

(defun def-use-attr-newer? (attr1 attr2)
  "Returns non-nil iff the modification time of `attr1' is later than the
modification time of `attr2'.  Note that this also returns nil when either
one of the modification times is nil."
  (and attr1 attr2
       (> (def-use-time-to-double (nth 5 attr1))
          (def-use-time-to-double (nth 5 attr2)))))

(defun def-use-attr-changed? (attr1 attr2)
  "Returns non-nil iff the file attributes of `attr1' are different than
the file attributes of `attr2'.  Note that this also returns nil when either
one of the file attributes is nil."
  (labels ((nequal (i) (not (equal (nth i attr1) (nth i attr2)))))
    (and attr1 attr2
         (or (def-use-attr-newer? attr1 attr2)
             (nequal 7)  ;; size
             (nequal 6)  ;; status change time
             (nequal 8)  ;; file modes
             (nequal 10) ;; inode
             ))))

(defun def-use-goto-line (line)
  "Goes to specified line quietly without setting mark.  By default, the
standard `goto-line' function in latest Gnu Emacs sets the mark displaying
the message \"Mark set\"."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-util)
