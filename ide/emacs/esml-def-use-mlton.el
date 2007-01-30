;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-mode)
(require 'sml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing of def-use -files produced by MLton.

(defvar esml-def-use-mlton-resolve-src-last-src nil)
(defvar esml-def-use-mlton-resolve-src-last-duf nil)
(defvar esml-def-use-mlton-resolve-src-last-result nil)

(defun esml-def-use-mlton-resolve-src (src duf)
  (if (and (equal esml-def-use-mlton-resolve-src-last-src src)
           (equal esml-def-use-mlton-resolve-src-last-duf duf))
      esml-def-use-mlton-resolve-src-last-result
    (setq esml-def-use-mlton-resolve-src-last-src src
          esml-def-use-mlton-resolve-src-last-duf duf
          esml-def-use-mlton-resolve-src-last-result
          (def-use-file-truename
            (cond
             ;; XXX <basis>
             ((file-name-absolute-p src)
              src)
             ((equal ?< (aref src 0))
              src)
             (t
              (expand-file-name
               src (file-name-directory duf))))))))

(defun esml-def-use-read (taking skipping)
  (let ((start (point)))
    (skip-chars-forward taking)
    (let ((result (buffer-substring start (point))))
      (skip-chars-forward skipping)
      result)))

(defconst esml-def-use-kinds
  `((,(def-use-intern "variable")    . ,font-lock-variable-name-face)
    (,(def-use-intern "type")        . ,font-lock-type-def-face)
    (,(def-use-intern "constructor") . ,font-lock-constant-face)
    (,(def-use-intern "structure")   . ,font-lock-module-def-face)
    (,(def-use-intern "signature")   . ,font-lock-interface-def-face)
    (,(def-use-intern "functor")     . ,font-lock-module-def-face)
    (,(def-use-intern "exception")   . ,font-lock-module-def-face)))

(defun esml-def-use-mlton-parse (duf)
  "Parses a def-use -file."
  (interactive "fSpecify def-use -file: ")
  (setq duf (expand-file-name duf))
  (with-temp-buffer
    (insert-file duf)
    (goto-char 1)
    (while (not (eobp))
      (let* ((kind (def-use-intern (esml-def-use-read "^ " " ")))
             (name (def-use-intern (esml-def-use-read "^ " " ")))
             (src (esml-def-use-mlton-resolve-src
                   (esml-def-use-read "^ " " ") duf))
             (line (string-to-int (esml-def-use-read "^." ".")))
             (col (- (string-to-int (esml-def-use-read "^\n" "\n")) 1))
             (pos (def-use-pos line col))
             (ref (def-use-ref src pos))
             (sym (def-use-sym kind name ref
                    (cdr (assoc kind esml-def-use-kinds)))))
        (def-use-add-def duf sym)
        (while (< 0 (skip-chars-forward " "))
          (let* ((src (esml-def-use-mlton-resolve-src
                       (esml-def-use-read "^ " " ") duf))
                 (line (string-to-int (esml-def-use-read "^." ".")))
                 (col (- (string-to-int (esml-def-use-read "^\n" "\n")) 1))
                 (pos (def-use-pos line col))
                 (ref (def-use-ref src pos)))
            (def-use-add-use ref sym)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-def-use-mlton)
