;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-mode)
(require 'sml-mode)
(require 'bg-job)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing of def-use -files produced by MLton.

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

(defun esml-du-mlton-parse (duf)
  "Parses a def-use -file.  Because parsing may take a while, it is
done as a background process.  This allows you to continue working
altough the editor may feel a bit sluggish."
  (interactive "fSpecify def-use -file: ")
  (setq duf (def-use-file-truename duf))
  (let ((buf (generate-new-buffer (concat "** " duf " **"))))
    (with-current-buffer buf
      (buffer-disable-undo buf)
      (insert-file duf)
      (goto-char 1)
      (setq buffer-read-only t))
    (message (concat "Parsing " duf " in the background..."))
    (bg-job-start
      (function
       (lambda (duf buf)
         (with-current-buffer buf
           (eobp))))
      (function
       (lambda (duf buf)
         (with-current-buffer buf
           (goto-char 1)
           (let* ((kind (def-use-intern (esml-du-read "^ " " ")))
                  (name (def-use-intern (esml-du-read "^ " " ")))
                  (src (def-use-file-truename
                         (esml-du-read "^ " " ")))
                  (line (string-to-int (esml-du-read "^." ".")))
                  (col (- (string-to-int (esml-du-read "^\n" "\n")) 1))
                  (pos (def-use-pos line col))
                  (ref (def-use-ref src pos))
                  (sym (def-use-sym kind name ref
                         (cdr (assoc kind esml-du-kinds)))))
             (def-use-add-def duf sym)
             (while (< 0 (skip-chars-forward " "))
               (let* ((src (def-use-file-truename
                             (esml-du-read "^ " " ")))
                      (line (string-to-int (esml-du-read "^." ".")))
                      (col (- (string-to-int (esml-du-read "^\n" "\n"))
                              1))
                      (pos (def-use-pos line col))
                      (ref (def-use-ref src pos)))
                 (def-use-add-use ref sym))))
           (setq buffer-read-only nil)
           (delete-backward-char (- (point) 1))
           (setq buffer-read-only t))
         (list duf buf)))
      (function
       (lambda (duf buf)
         (kill-buffer buf)
         (message (concat "Finished parsing " duf "."))))
      duf buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-du-mlton)
