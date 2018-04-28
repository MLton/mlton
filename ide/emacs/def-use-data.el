;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-sym)

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

(defun def-use-sym (class msg name ref &optional face)
  "Symbol constructor."
  (cons ref (cons name (cons class (cons msg face)))))
(defalias 'def-use-sym-face (function cddddr))
(defalias 'def-use-sym-msg (function cadddr))
(defalias 'def-use-sym-class (function caddr))
(defalias 'def-use-sym-name (function cadr))
(defalias 'def-use-sym-ref (function car))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Def-use sources

(defun def-use-add-dus (title sym-at-ref sym-to-uses finalize attr &rest args)
  (push `(,args ,sym-at-ref ,sym-to-uses ,attr ,title . ,finalize)
        def-use-dus-list)
  (def-use-show-dus-update))

(defun def-use-rem-dus (dus)
  (setq def-use-dus-list
        (remove dus def-use-dus-list))
  (def-use-dus-finalize dus)
  (def-use-show-dus-update))

(defun def-use-dus-sym-at-ref (dus ref)
  (apply (cadr dus) ref (car dus)))

(defun def-use-dus-sym-to-uses (dus sym)
  (apply (caddr dus) sym (car dus)))

(defun def-use-dus-attr (dus)
  (apply (cadddr dus) (car dus)))

(defun def-use-dus-title (dus)
  (apply (cadddr (cdr dus)) (car dus)))

(defun def-use-dus-finalize (dus)
  (apply (cddddr (cdr dus)) (car dus)))

(defvar def-use-dus-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Def-Use Sources -mode

(defconst def-use-show-dus-buffer-name "<:Def-Use Sources:>")

(defconst def-use-show-dus-mode-map
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result
               (read (car key-command))
               (cdr key-command))))
          `(("[(q)]"
             . ,(function def-use-kill-current-buffer))
            ("[(k)]"
             . ,(function def-use-show-dus-del))))
    result))

(define-derived-mode def-use-show-dus-mode fundamental-mode "Def-Use-DUS"
  "Major mode for browsing def-use sources."
  :group 'def-use-dus)

(defun def-use-show-dus ()
  "Show a list of def-use sources."
  (interactive)
  (let ((buffer (get-buffer-create def-use-show-dus-buffer-name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq buffer-read-only t)
      (def-use-show-dus-mode))
    (switch-to-buffer buffer))
  (def-use-show-dus-update))

(defun def-use-show-dus-update ()
  (let ((buffer (get-buffer def-use-show-dus-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((point (point)))
          (setq buffer-read-only nil)
          (goto-char 1)
          (delete-char (buffer-size))
          (insert "Def-Use Sources\n"
                  "\n")
          (mapc (function
                 (lambda (dus)
                   (insert (def-use-dus-title dus) "\n")))
                def-use-dus-list)
          (setq buffer-read-only t)
          (goto-char point))))))

(defun def-use-show-dus-del ()
  "Kill the def-use source on the current line."
  (interactive)
  (let ((idx (- (def-use-current-line) 3)))
    (when (and (<= 0 idx)
               (< idx (length def-use-dus-list)))
      (def-use-rem-dus (nth idx def-use-dus-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries

(defun def-use-attrs ()
  (sort (mapcar (function def-use-dus-attr)
                def-use-dus-list)
        (function def-use-attr-newer?)))

(defun def-use-query (fn)
  "Queries the def-use -sources with the given function and moves the
satisfied dus to the front."
  (let ((prev nil)
        (work def-use-dus-list)
        (result nil))
    (while (and work
                (not (setq result (funcall fn (car work)))))
      (setq prev work)
      (setq work (cdr work)))
    (when (and prev work)
      (setcdr prev (cdr work))
      (setcdr work def-use-dus-list)
      (setq def-use-dus-list work)
      (def-use-show-dus-update))
    result))

(defun def-use-sym-at-ref (ref &optional no-apology)
  (when ref
    (let ((sym
           (def-use-query
             (function
              (lambda (dus)
                (def-use-dus-sym-at-ref dus ref)))))
          (name (def-use-extract-sym-name-at-ref ref)))
      (if (and sym name (string= (def-use-sym-name sym) name))
          sym
        (unless no-apology
          (cond
           ((not name)
            (message "Point does not appear to be on a symbol."))
           ((and sym (not (string= (def-use-sym-name sym) name)))
            (message
             "Symbol at point, %s, does not match, %s, in info.  Check mode."
             name
             (def-use-sym-name sym)))
           (t
            (let* ((attrs (def-use-attrs))
                   (file (def-use-ref-src ref))
                   (attr (file-attributes file))
                   (buffer (def-use-find-buffer-visiting-file file)))
              (message
               "Sorry, no valid info on the symbol: %s.  Possible reason: %s."
               name
               (cond
                ((not attrs)
                 "There are no def-use sources")
                ((def-use-attr-newer? attr (car attrs))
                 "The file is newer than any def-use source")
                ((buffer-modified-p buffer)
                 "The buffer has been modified")
                (t
                 "The symbol may not be in any def-use source")))))))
        nil))))

(defun def-use-sym-to-uses (sym)
  (when sym
    (def-use-query
      (function
       (lambda (dus)
         (def-use-dus-sym-to-uses dus sym))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-data)
