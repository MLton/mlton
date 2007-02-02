;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-mode)
(require 'bg-job)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defun esml-du-mlton (duf)
  "Gets def-use information from a def-use file produced by MLton."
  (interactive "fSpecify def-use -file: ")
  (let ((ctx (esml-du-ctx (def-use-file-truename duf))))
    (esml-du-parse ctx)
    (def-use-add-dus
      (def-use-dus
        (function esml-du-title)
        (function esml-du-sym-at-ref)
        (function esml-du-sym-to-uses)
        (function esml-du-finalize)
        ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods

(defun esml-du-title (ctx)
  (esml-du-ctx-duf ctx))

(defun esml-du-sym-at-ref (ref ctx)
  (if (def-use-attr-newer?
        (file-attributes (def-use-ref-src ref))
        (esml-du-ctx-attr ctx))
      (esml-du-reparse ctx)
    (gethash ref (esml-du-ctx-ref-to-sym-table ctx))))

(defun esml-du-sym-to-uses (sym ctx)
  (if (def-use-attr-newer?
        (file-attributes (def-use-ref-src (def-use-sym-ref sym)))
        (esml-du-ctx-attr ctx))
      (esml-du-reparse ctx)
    (gethash sym (esml-du-ctx-sym-to-uses-table ctx))))

(defun esml-du-finalize (ctx)
  (when (esml-du-ctx-buf ctx)
    (with-current-buffer (esml-du-ctx-buf ctx)
      (setq buffer-read-only nil)
      (goto-char 1)
      (delete-char (buffer-size))
      (setq buffer-read-only t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context

(defun esml-du-ctx (duf)
  (cons (def-use-make-hash-table)
        (cons (def-use-make-hash-table)
              (cons duf
                    (cons nil nil)))))

(defalias 'esml-du-ctx-buf               (function cddddr))
(defalias 'esml-du-ctx-attr              (function cadddr))
(defalias 'esml-du-ctx-duf               (function caddr))
(defalias 'esml-du-ctx-ref-to-sym-table  (function cadr))
(defalias 'esml-du-ctx-sym-to-uses-table (function car))

(defun esml-du-ctx-set-buf  (buf  ctx) (setcdr (cdddr ctx) buf))
(defun esml-du-ctx-set-attr (attr ctx) (setcar (cdddr ctx) attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defun esml-du-read (taking skipping)
  (let ((start (point)))
    (skip-chars-forward taking)
    (let ((result (buffer-substring start (point))))
      (skip-chars-forward skipping)
      result)))

(defconst esml-du-kinds ;; XXX Needs customization
  `((,(def-use-intern "variable")    . ,font-lock-variable-name-face)
    (,(def-use-intern "type")        . ,font-lock-variable-name-face)
    (,(def-use-intern "constructor") . ,font-lock-variable-name-face)
    (,(def-use-intern "structure")   . ,font-lock-variable-name-face)
    (,(def-use-intern "signature")   . ,font-lock-variable-name-face)
    (,(def-use-intern "functor")     . ,font-lock-variable-name-face)
    (,(def-use-intern "exception")   . ,font-lock-variable-name-face)))

(defun esml-du-reparse (ctx)
  (cond
   ((not (def-use-attr-newer?
           (file-attributes (esml-du-ctx-duf ctx))
           (esml-du-ctx-attr ctx)))
    nil)
   ((not (esml-du-ctx-buf ctx))
    (esml-du-parse ctx)
    nil)
   (t
    (esml-du-finalize ctx)
    (run-with-idle-timer 0.1 nil (function esml-du-reparse) ctx)
    nil)))

(defun esml-du-parse (ctx)
  "Parses the def-use -file.  Because parsing may take a while, it is
done as a background process.  This allows you to continue working
altough the editor may feel a bit sluggish."
  (esml-du-ctx-set-attr (file-attributes (esml-du-ctx-duf ctx)) ctx)
  (esml-du-ctx-set-buf
   (generate-new-buffer (concat "** " (esml-du-ctx-duf ctx) " **")) ctx)
  (with-current-buffer (esml-du-ctx-buf ctx)
    (buffer-disable-undo)
    (insert-file (esml-du-ctx-duf ctx))
    (setq buffer-read-only t)
    (goto-char 1))
  (clrhash (esml-du-ctx-ref-to-sym-table ctx))
  (clrhash (esml-du-ctx-sym-to-uses-table ctx))
  (bg-job-start
   (function
    (lambda (ctx)
      (with-current-buffer (esml-du-ctx-buf ctx)
        (eobp))))
   (function
    (lambda (ctx)
      (with-current-buffer (esml-du-ctx-buf ctx)
        (goto-char 1)
        (let* ((ref-to-sym (esml-du-ctx-ref-to-sym-table ctx))
               (sym-to-uses (esml-du-ctx-sym-to-uses-table ctx))
               (kind (def-use-intern (esml-du-read "^ " " ")))
               (name (def-use-intern (esml-du-read "^ " " ")))
               (src (def-use-file-truename (esml-du-read "^ " " ")))
               (line (string-to-int (esml-du-read "^." ".")))
               (col (- (string-to-int (esml-du-read "^\n" "\n")) 1))
               (pos (def-use-pos line col))
               (ref (def-use-ref src pos))
               (sym (def-use-sym kind name ref
                      (cdr (assoc kind esml-du-kinds)))))
          (puthash ref sym ref-to-sym)
          (while (< 0 (skip-chars-forward " "))
            (let* ((src (def-use-file-truename (esml-du-read "^ " " ")))
                   (line (string-to-int (esml-du-read "^." ".")))
                   (col (- (string-to-int (esml-du-read "^\n" "\n")) 1))
                   (pos (def-use-pos line col))
                   (ref (def-use-ref src pos)))
              (puthash ref sym (esml-du-ctx-ref-to-sym-table ctx))
              (puthash sym (cons ref (gethash sym sym-to-uses))
                       sym-to-uses))))
        (setq buffer-read-only nil)
        (delete-backward-char (- (point) 1))
        (setq buffer-read-only t))
      (list ctx)))
   (function
    (lambda (ctx)
      (kill-buffer (esml-du-ctx-buf ctx))
      (esml-du-ctx-set-buf nil ctx)
      (message (concat "Finished parsing " (esml-du-ctx-duf ctx) "."))))
   ctx)
  (message (concat "Parsing " (esml-du-ctx-duf ctx) " in the background...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-du-mlton)
