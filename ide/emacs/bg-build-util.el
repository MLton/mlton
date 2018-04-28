;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'cl)
(require 'compat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun bg-build-cons-once (entry list)
  (cons entry (remove* entry list :test (function equal))))

(defun bg-build-flatmap (fn list)
  (apply (function append) (mapcar fn list)))

(defun bg-build-remove-from-assoc (alist key)
  (remove*
   nil alist
   :test (function
          (lambda (_ key-value)
            (equal key (car key-value))))))

(defun bg-build-replace-in-assoc (alist key value)
  (cons (cons key value)
        (bg-build-remove-from-assoc alist key)))

(defun bg-build-assoc-cdr (key alist)
  "Same as (cdr (assoc key alist)) except that doesn't attempt to call cdr
on nil."
  (let ((key-value (assoc key alist)))
    (when key-value
      (cdr key-value))))

(defun bg-build-const (value)
  "Returns a function that returns the given value."
  (lexical-let ((value value))
    (lambda (&rest _)
      value)))

(defun bg-build-kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun bg-build-make-hash-table ()
  "Makes a hash table with `equal' semantics."
  (make-hash-table :test 'equal :size 1))

(defun bg-build-point-at-current-line ()
  "Returns point at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun bg-build-current-line ()
  "Returns the current line number counting from 1."
  (+ 1 (count-lines 1 (bg-build-point-at-current-line))))

(defun bg-build-time-to-double (time)
  "Converts a time to a double."
  (+ (* (car time) 65536.0)
     (cadr time)
     (if (cddr time) (* (caddr time) 1e-06) 0)))

(defun bg-build-attr-newer? (attr1 attr2)
  "Returns non-nil iff the modification time of `attr1' is later than the
modification time of `attr2'.  Note that this also returns nil when either
one of the modification times is nil."
  (and attr1 attr2
       (> (bg-build-time-to-double (nth 5 attr1))
          (bg-build-time-to-double (nth 5 attr2)))))

(defun bg-build-pos-to-point (pos)
  "Returns the value of point in the current buffer at the position given
as a (line . col) pair."
  (save-excursion
    (goto-line (car pos))
    (+ (point) (cdr pos))))

(defun bg-build-point-to-pos (point)
  "Returns the position as a (line . col) pair corresponding to the
specified point in the current buffer."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (let ((line (+ (count-lines 1 (point)) 1))
          (col (- point (point))))
      (cons line col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bg-build-util)
