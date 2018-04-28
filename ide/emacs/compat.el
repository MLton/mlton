;; Copyright (C) 2007-2008 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnu Emacs / XEmacs compatibility workarounds

(if (string-match "XEmacs" emacs-version)
    (defun compat-replace-regexp-in-string (str regexp rep)
      (replace-in-string str regexp rep t))
  (defun compat-replace-regexp-in-string (str regexp rep)
    (replace-regexp-in-string regexp rep str t t)))

(if (string-match "XEmacs" emacs-version)
    (defun compat-error (str &rest objs)
      (error 'error (concat "Error: " (apply (function format) str objs) ".")))
  (defalias 'compat-error (function error)))

(if (string-match "XEmacs" emacs-version)
    (defalias 'compat-add-local-hook (function add-local-hook))
  (defun compat-add-local-hook (hook fn)
    (add-hook hook fn nil t)))

(if (string-match "XEmacs" emacs-version)
    (defun compat-abbreviate-file-name (file)
      (abbreviate-file-name file t))
  (defalias 'compat-abbreviate-file-name (function abbreviate-file-name)))

(if (string-match "XEmacs" emacs-version)
    (defalias 'compat-delete-timer (function delete-itimer))
  (defalias 'compat-delete-timer (function cancel-timer)))

(if (string-match "XEmacs" emacs-version)
    (defalias 'compat-read-file-name (function read-file-name))
  (defun compat-read-file-name (&optional a b c d e f)
    (funcall (function read-file-name) a b c d e)))

(if (string-match "XEmacs" emacs-version)
    (defalias 'compat-process-live-p (function process-live-p))
  (defun compat-process-live-p (process)
    (case (process-status process)
      ((run stop) t))))

(if (string-match "XEmacs" emacs-version)
    (defun compat-compilation-parse-errors ()
      (funcall compilation-parse-errors-function nil nil))
  (defun compat-compilation-parse-errors ()
    (compilation-compat-parse-errors (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'compat)
