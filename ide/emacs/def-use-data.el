;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-util)

;; XXX Improve database design
;;
;; This hash table based database design isn't very flexible.  In
;; particular, it would be inefficient to update the database after a
;; buffer change.  There are data structures that would make such
;; updates feasible.  Look at overlays in Emacs, for example.
;;
;; Also, instead of loading the def-use -file to memory, which takes a
;; lot of time and memory, it might be better to query the file in
;; real-time.  On my laptop, it takes less than a second to grep
;; through MLton's def-use -file and about 1/25 when the files are in
;; cache.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data records

(defalias 'def-use-pos (function cons))
(defalias 'def-use-pos-line (function car))
(defalias 'def-use-pos-col  (function cdr))

(defun def-use-ref (src pos)
  "Reference constructor."
  (cons (def-use-intern src) pos))
(defalias 'def-use-ref-src (function car))
(defalias 'def-use-ref-pos (function cdr))

(defun def-use-sym (kind name ref)
  "Symbol constructor."
  (cons ref (cons (def-use-intern name) (def-use-intern kind))))
(defun def-use-sym-kind (sym) (cddr sym))
(defun def-use-sym-name (sym) (cadr sym))
(defalias 'def-use-sym-ref (function car))

(defun def-use-info ()
  "Info constructor."
  (cons (def-use-make-hash-table) (def-use-make-hash-table)))
(defalias 'def-use-info-pos-to-sym (function car))
(defalias 'def-use-info-sym-set    (function cdr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data tables

(defvar def-use-duf-to-src-set-table (def-use-make-hash-table)
  "Maps a def-use -file to a set of sources.")

(defvar def-use-src-to-info-table (def-use-make-hash-table)
  "Maps a source to a source info.")

(defvar def-use-sym-to-use-set-table (def-use-make-hash-table)
  "Maps a symbol to a set of references to the symbol.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data entry

(defun def-use-add-def (duf sym)
  "Adds the definition of the specified symbol."
  (let* ((ref (def-use-sym-ref sym))
         (src (def-use-ref-src ref))
         (info (def-use-src-to-info src)))
    (puthash ref ref (def-use-sym-to-use-set sym))
    (puthash src src (def-use-duf-to-src-set duf))
    (puthash sym sym (def-use-info-sym-set info))
    (puthash (def-use-ref-pos ref) sym (def-use-info-pos-to-sym info))))

(defun def-use-add-use (ref sym)
  "Adds a reference to (use of) the specified symbol."
  (puthash ref ref (def-use-sym-to-use-set sym))
  (puthash (def-use-ref-pos ref) sym
           (def-use-src-to-pos-to-sym (def-use-ref-src ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data access

(defun def-use-duf-to-src-set (duf)
  "Returns the existing source set for the specified def-use -file or a
new empty set."
  (def-use-gethash-or-put duf (function def-use-make-hash-table)
    def-use-duf-to-src-set-table))

(defun def-use-sym-to-use-set (sym)
  "Returns the existing use set for the specified symbol or a new empty
use set."
  (def-use-gethash-or-put sym (function def-use-make-hash-table)
    def-use-sym-to-use-set-table))

(defun def-use-src-to-info (src)
  "Returns the existing source info for the specified source or a new
empty source info."
  (def-use-gethash-or-put src (function def-use-info)
    def-use-src-to-info-table))

(defun def-use-duf-to-srcs (duf)
  "Returns a list of all sources whose symbols the def-use -file describes."
  (def-use-set-to-list (def-use-duf-to-src-set duf)))

(defun def-use-src-to-pos-to-sym (src)
  "Returns a position to symbol table for the specified source."
  (def-use-info-pos-to-sym (def-use-src-to-info src)))

(defun def-use-src-to-sym-set (src)
  "Returns a set of all symbols defined in the specified source."
  (def-use-info-sym-set (def-use-src-to-info src)))

(defun def-use-sym-at-ref (ref)
  "Returns the symbol referenced at specified ref."
  (gethash (def-use-ref-pos ref)
           (def-use-src-to-pos-to-sym (def-use-ref-src ref))))

(defun def-use-src-to-syms (src)
  "Returns a list of symbols defined (not symbols referenced) in the
specified source."
  (def-use-set-to-list (def-use-src-to-sym-set src)))

(defun def-use-sym-to-uses (sym)
  "Returns a list of uses of the specified symbol."
  (def-use-set-to-list (def-use-sym-to-use-set sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data purging

(defun def-use-purge-all ()
  "Purges all data cached by def-use -mode."
  (interactive)
  (setq def-use-duf-to-src-set-table (def-use-make-hash-table))
  (setq def-use-src-to-info-table (def-use-make-hash-table))
  (setq def-use-sym-to-use-set-table (def-use-make-hash-table)))

;; XXX Ability to purge data in a more fine grained manner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-data)
