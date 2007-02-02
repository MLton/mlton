;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data records

(defalias 'def-use-pos (function cons))
(defalias 'def-use-pos-line (function car))
(defalias 'def-use-pos-col  (function cdr))
(defun def-use-pos< (lhs rhs)
  (or (< (def-use-pos-line lhs) (def-use-pos-line rhs))
      (and (equal (def-use-pos-line lhs) (def-use-pos-line rhs))
           (< (def-use-pos-col lhs) (def-use-pos-col rhs)))))

(defalias 'def-use-ref (function cons))
(defalias 'def-use-ref-src (function car))
(defalias 'def-use-ref-pos (function cdr))
(defun def-use-ref< (lhs rhs)
  (or (string< (def-use-ref-src lhs) (def-use-ref-src rhs))
      (and (equal (def-use-ref-src lhs) (def-use-ref-src rhs))
           (def-use-pos< (def-use-ref-pos lhs) (def-use-ref-pos rhs)))))

(defun def-use-sym (kind name ref &optional face)
  "Symbol constructor."
  (cons ref (cons name (cons kind face))))
(defalias 'def-use-sym-face (function cdddr))
(defalias 'def-use-sym-kind (function caddr))
(defalias 'def-use-sym-name (function cadr))
(defalias 'def-use-sym-ref (function car))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Def-use source

(defun def-use-dus (title sym-at-ref sym-to-uses finalize &rest args)
  "Makes a new def-use -source."
  (cons args (cons sym-at-ref (cons sym-to-uses (cons title finalize)))))

(defun def-use-dus-sym-at-ref (dus ref)
  (apply (cadr dus) ref (car dus)))

(defun def-use-dus-sym-to-uses (dus sym)
  (apply (caddr dus) sym (car dus)))

(defun def-use-dus-title (dus)
  (apply (cadddr dus) (car dus)))

(defun def-use-dus-finalize (dus)
  (apply (cddddr dus) (car dus)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Def-use source list

(defvar def-use-dus-list nil
  "List of active def-use sources.")

(defun def-use-add-dus (dus)
  (push dus def-use-dus-list))

(defun def-use-rem-dus (dus)
  (setq def-use-dus-list
        (remove dus def-use-dus-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries

(defun def-use-sym-at-ref (ref)
  (when ref
    (loop for dus in def-use-dus-list do
      (let ((it (def-use-dus-sym-at-ref dus ref)))
        (when it (return it))))))

(defun def-use-sym-to-uses (sym)
  (when sym
    (loop for dus in def-use-dus-list do
      (let ((it (def-use-dus-sym-to-uses dus sym)))
        (when it (return it))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-data)
