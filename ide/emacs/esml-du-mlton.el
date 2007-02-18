;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-mode)
(require 'bg-job)
(require 'esml-util)

;; XXX Keep a set of files covered by a def-use file.  Don't reload unnecessarily.
;; XXX Detect when the same ref is both a use and a def and act appropriately.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup esml-du nil
  "MLton def-use info plugin for `def-use-mode'."
  :group 'sml)

(defcustom esml-du-change-poll-period 1.0
  "Delay in seconds between file change polls."
  :type '(choice (number :tag "Period in seconds")
                 (const :tag "Disable polling" nil))
  :group 'esml-du)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defun esml-du-mlton (duf)
  "Gets def-use information from a def-use file produced by MLton."
  (interactive "fSpecify def-use -file: ")
  (let ((ctx (esml-du-ctx (def-use-file-truename duf))))
    (esml-du-parse ctx)
    (def-use-add-dus
      (function esml-du-title)
      (function esml-du-sym-at-ref)
      (function esml-du-sym-to-uses)
      (function esml-du-finalize)
      ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move to symbol

(defun esml-du-move-to-symbol-start ()
  "Moves to the start of the SML symbol at point."
  (let ((limit (def-use-point-at-current-line)))
    (when (zerop (skip-chars-backward esml-sml-alphanumeric-chars limit))
      (skip-chars-backward esml-sml-symbolic-chars limit))))

(add-to-list 'def-use-mode-to-move-to-symbol-start-alist
             (cons 'sml-mode (function esml-du-move-to-symbol-start)))

(defun esml-du-move-to-symbol-end ()
  "Moves to the end of the SML symbol at point."
  (let ((limit (def-use-point-at-next-line)))
    (when (zerop (skip-chars-forward esml-sml-alphanumeric-chars limit))
      (skip-chars-forward esml-sml-symbolic-chars limit))))

(add-to-list 'def-use-mode-to-move-to-symbol-end-alist
             (cons 'sml-mode (function esml-du-move-to-symbol-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods

(defun esml-du-title (ctx)
  (concat
   (esml-du-ctx-duf ctx)
   " ["
   (if (esml-du-ctx-buf ctx)
       (concat "parsing: "
               (int-to-string
                (truncate
                 (/ (buffer-size (esml-du-ctx-buf ctx))
                    0.01
                    (nth 7 (esml-du-ctx-attr ctx)))))
               "% left")
     "complete")
   ", parsed "
   (int-to-string (esml-du-ctx-parse-cnt ctx))
   " times]"))

(defun esml-du-sym-at-ref (ref ctx)
  (if (def-use-attr-newer?
        (file-attributes (def-use-ref-src ref))
        (esml-du-ctx-attr ctx))
      (esml-du-reparse ctx)
    (unless (let ((buffer (def-use-find-buffer-visiting-file
                            (def-use-ref-src ref))))
              (and buffer (buffer-modified-p buffer)))
      (or (gethash ref (esml-du-ctx-ref-to-sym-table ctx))
          (and (esml-du-try-to-read-symbol-at-ref ref ctx)
               (gethash ref (esml-du-ctx-ref-to-sym-table ctx)))))))

(defun esml-du-sym-to-uses (sym ctx)
  (if (def-use-attr-newer?
        (file-attributes (def-use-ref-src (def-use-sym-ref sym)))
        (esml-du-ctx-attr ctx))
      (esml-du-reparse ctx)
    (gethash sym (esml-du-ctx-sym-to-uses-table ctx))))

(defun esml-du-stop-parsing (ctx)
  (let ((buffer (esml-du-ctx-buf ctx)))
    (when buffer
      (kill-buffer buffer))))

(defun esml-du-finalize (ctx)
  (esml-du-stop-parsing ctx)
  (let ((timer (esml-du-ctx-poll-timer ctx)))
    (when timer
      (compat-delete-timer timer)
      (esml-du-ctx-set-poll-timer nil ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context

(defun esml-du-ctx (duf)
  (let ((ctx (vector (def-use-make-hash-table) (def-use-make-hash-table)
                     duf nil nil nil 0)))
    (when esml-du-change-poll-period
      (esml-du-ctx-set-poll-timer
       (run-with-timer esml-du-change-poll-period esml-du-change-poll-period
                       (function esml-du-reparse) ctx)
       ctx))
    ctx))

(defun esml-du-ctx-parse-cnt         (ctx) (aref ctx 6))
(defun esml-du-ctx-poll-timer        (ctx) (aref ctx 5))
(defun esml-du-ctx-buf               (ctx) (aref ctx 4))
(defun esml-du-ctx-attr              (ctx) (aref ctx 3))
(defun esml-du-ctx-duf               (ctx) (aref ctx 2))
(defun esml-du-ctx-ref-to-sym-table  (ctx) (aref ctx 1))
(defun esml-du-ctx-sym-to-uses-table (ctx) (aref ctx 0))

(defun esml-du-ctx-inc-parse-cnt  (ctx)
  (aset ctx 6 (1+ (aref ctx 6))))

(defun esml-du-ctx-set-poll-timer (timer ctx) (aset ctx 5 timer))
(defun esml-du-ctx-set-buf        (buf   ctx) (aset ctx 4 buf))
(defun esml-du-ctx-set-attr       (attr  ctx) (aset ctx 3 attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defun esml-du-read (taking skipping)
  (let ((start (point)))
    (skip-chars-forward taking)
    (let ((result (buffer-substring start (point))))
      (skip-chars-forward skipping)
      result)))

(defconst esml-du-classes ;; XXX Needs customization
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
    (esml-du-stop-parsing ctx)
    (run-with-idle-timer 0.5 nil (function esml-du-reparse) ctx)
    nil)))

(defun esml-du-try-to-read-symbol-at-ref (ref ctx)
  "Tries to read the symbol at the specified ref from the duf."
  (let ((buffer (esml-du-ctx-buf ctx)))
    (when buffer
      (with-current-buffer buffer
        (goto-char 1)
        (when (search-forward (esml-du-ref-to-appx-syntax ref) nil t)
          (beginning-of-line)
          (while (= ?\  (char-after))
            (forward-line -1))
          (let ((start (point)))
            (esml-du-read-one-symbol ctx)
            (setq buffer-read-only nil)
            (delete-backward-char (- (point) start))
            (setq buffer-read-only t)))))))

(defun esml-du-ref-to-appx-syntax (ref)
  (let ((pos (def-use-ref-pos ref)))
    (concat
     (file-name-nondirectory (def-use-ref-src ref)) " "
     (int-to-string (def-use-pos-line pos)) "."
     (int-to-string (1+ (def-use-pos-col pos))))))

(defun esml-du-read-one-symbol (ctx)
  "Reads one symbol from the current buffer starting at the current
point."
  (let* ((ref-to-sym (esml-du-ctx-ref-to-sym-table ctx))
         (sym-to-uses (esml-du-ctx-sym-to-uses-table ctx))
         (class (def-use-intern (esml-du-read "^ " " ")))
         (name (def-use-intern (esml-du-read "^ " " ")))
         (src (def-use-file-truename (esml-du-read "^ " " ")))
         (line (string-to-int (esml-du-read "^." ".")))
         (col (1- (string-to-int (esml-du-read "^\n" "\n"))))
         (pos (def-use-pos line col))
         (ref (def-use-ref src pos))
         (sym (def-use-sym class name ref
                (cdr (assoc class esml-du-classes))))
         (uses nil))
    (puthash ref sym ref-to-sym)
    (while (< 0 (skip-chars-forward " "))
      (let* ((src (def-use-file-truename (esml-du-read "^ " " ")))
             (line (string-to-int (esml-du-read "^." ".")))
             (col (1- (string-to-int (esml-du-read "^\n" "\n"))))
             (pos (def-use-pos line col))
             (ref (def-use-ref src pos)))
        (puthash ref sym (esml-du-ctx-ref-to-sym-table ctx))
        (push ref uses)))
    (puthash sym uses sym-to-uses)))

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
    (goto-char 1)
    (compat-add-local-hook
     'kill-buffer-hook
     (lexical-let ((ctx ctx))
       (function
        (lambda ()
          (esml-du-ctx-set-buf nil ctx))))))
  (clrhash (esml-du-ctx-ref-to-sym-table ctx))
  (clrhash (esml-du-ctx-sym-to-uses-table ctx))
  (garbage-collect)
  (bg-job-start
   (function
    (lambda (ctx)
      (let ((buffer (esml-du-ctx-buf ctx)))
        (or (not buffer)
            (with-current-buffer buffer
              (goto-char 1)
              (eobp))))))
   (function
    (lambda (ctx)
      (with-current-buffer (esml-du-ctx-buf ctx)
        (goto-char 1)
        (esml-du-read-one-symbol ctx)
        (setq buffer-read-only nil)
        (delete-backward-char (1- (point)))
        (setq buffer-read-only t))))
   (function
    (lambda (ctx)
      (esml-du-stop-parsing ctx)
      (esml-du-ctx-inc-parse-cnt ctx)
      (message "Finished parsing %s." (esml-du-ctx-duf ctx))))
   ctx)
  (message "Parsing %s in the background..." (esml-du-ctx-duf ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-du-mlton)
