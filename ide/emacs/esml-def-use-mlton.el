;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-mode)

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

(defun esml-def-use-mlton-parse (duf)
  "Parses a def-use -file."
  (interactive "fSpecify def-use -file: ")
  (setq duf (expand-file-name duf))
  (with-temp-buffer
    (insert-file duf)
    (goto-char 1)
    (while (not (eobp))
      (let* ((kind (esml-def-use-read "^ " " "))
             (name (esml-def-use-read "^ " " "))
             (src (esml-def-use-mlton-resolve-src
                   (esml-def-use-read "^ " " ") duf))
             (line (string-to-int (esml-def-use-read "^." ".")))
             (col (- (string-to-int (esml-def-use-read "^\n" "\n")) 1))
             (pos (def-use-pos line col))
             (ref (def-use-ref src pos))
             (sym (def-use-sym kind name ref)))
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
