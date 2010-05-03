;; This code defines a few functions for invoking MLton's type checker and
;; visiting the resulting errors.  The intended use is:
;;
;;  1. Call mlton-set-main while visiting your main .mlb file.
;;  2. Call mlton-compile to invoke MLton and visit the first error.
;;  3. Repeatedly call mlton-next-error to visit each error.
;;
;; Calling mlton-compile waits until MLton's type checker completes before
;; visiting the first error.  One nice thing is that mlton-parse-errors uses
;; markers so that file edits don't interfere with locating subsequent errros.

(setq mlton-command "mlton")
(setq mlton-flags "")
(setq mlton-main-file "mlton-main-file undefined")
(setq mlton-output-buffer "*mlton-output*")
(setq mlton-errors nil)
(setq mlton-error-regexp
      "^\\(Error\\|Warning\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\).")

(defun mlton-parse-errors (prefix buffer)
  "Parse a sequence of MLton error messages in buffer.  prefix is the path
relative to which files in the error messages should be interpreted.
Returns a list of pairs of the form (pos . marker), where pos is a position
in buffer at the start of the second line of an error message (i.e. after the
file, line, and column info) and marker is at the point of the error in the
source file."
  (if (not (get-buffer buffer))
      (message "No errors.")
    (let ((errors ()))
      (set-buffer buffer)
      (goto-char 0)
      (condition-case ()
        (while t
          (re-search-forward mlton-error-regexp)
          (let* ((match (lambda (i)
                          (buffer-substring (match-beginning i)
                                            (match-end i))))
                 (file (funcall match 2))
                 (file (if (file-name-absolute-p file)
                           file
                         (concat prefix (funcall match 2))))
                 (line (string-to-int (funcall match 3)))
                 (col (string-to-int (funcall match 4)))
                 (marker (save-excursion
                           (find-file file)
                           (goto-line line)
                           (forward-char (- col 1))
                           (set-marker (make-marker) (point)))))
            (beginning-of-line)
            (forward-line)
            (setq errors (cons (cons (point) marker) errors))))
        (error))
        (setq mlton-errors (reverse errors)))))

(defun mlton-next-error ()
  (interactive)
  (if (or (not (get-buffer mlton-output-buffer))
          (null mlton-errors))
      (message "No more errors.")
    (let ((error (caar mlton-errors))
          (marker (cdar mlton-errors)))
      (setq mlton-errors (cdr mlton-errors))
      (set-window-start (display-buffer mlton-output-buffer) error)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

(defun mlton-set-main ()
  (interactive)
  (setq mlton-main-file (buffer-file-name)))

(defvar sml-filename-regexp
   "\\(\\([-a-zA-Z0-9/.]\\)+\\)\\(\\.\\)\\(\\(cm\\)\\|\\(fun\\)\\|\\(grm\\)\\|\\(lex\\)\\|\\(mlb\\)\\|\\(sig\\)\\|\\(sml\\)\\|\\(ML\\)\\)")

(defmacro save-buffer-excursion (&rest exps)
  `(let ((old-buffer (current-buffer)))
     (,@ exps)
     (set-buffer old-buffer)))

(defun sml-save-buffers ()
  (save-buffer-excursion
   (let ((l (buffer-list)))
     (while (not (null l))
       (let* ((b (car l))
              (n (buffer-name b)))
         (if (and n (string-match sml-filename-regexp n))
             (progn
               (set-buffer b)
               (if (buffer-modified-p) (save-buffer)))))
       (setq l (cdr l))))))

(defun mlton-compile ()
  (interactive)
  (let ((buffer (current-buffer)))
    (sml-save-buffers)
    (if (get-buffer mlton-output-buffer)
        (kill-buffer mlton-output-buffer))
    (find-file mlton-main-file)
    (shell-command (concat mlton-command
                           " " mlton-flags " "
                           " -stop tc "
                           (file-name-nondirectory mlton-main-file))
                   mlton-output-buffer)
    (mlton-parse-errors (file-name-directory mlton-main-file)
                        mlton-output-buffer)
    (switch-to-buffer buffer)
    (mlton-next-error)))

(defun mlton-parse-errors-this-buffer ()
  (interactive)
  (if (get-buffer mlton-output-buffer)
      (kill-buffer mlton-output-buffer))
  (rename-buffer mlton-output-buffer)
  (mlton-parse-errors (file-name-directory mlton-main-file)
                      (current-buffer)))
  
(provide 'sml-mlton)
