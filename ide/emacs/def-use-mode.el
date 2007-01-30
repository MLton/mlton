;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

;; TBD:
;; - automatic loading of def-use files
;; - make loading of def-use files asynchronous
;; - disable def-use when file is modified

(require 'def-use-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup def-use nil
  "Minor mode to support precisely identified definitions and uses."
  :group 'matching)

(defface def-use-def-face
  '((((class color)) (:background "paleturquoise3"))
    (t (:background "gray")))
  "Face for highlighting definitions."
  :group 'faces
  :group 'def-use)

(defface def-use-use-face
  '((((class color)) (:background "darkseagreen3"))
    (t (:background "gray")))
  "Face for highlighting uses."
  :group 'faces
  :group 'def-use)

(defcustom def-use-delay 0.125
  "Idle time in seconds to delay before updating highlighting."
  :type '(number :tag "seconds")
  :group 'def-use)

(defcustom def-use-priority 1000
  "Priority of highlighting overlays."
  :type 'integer
  :group 'def-use)

(defcustom def-use-key-bindings
  '(("[(control c) (control d)]"
     . def-use-jump-to-def)
    ("[(control c) (control n)]"
     . def-use-jump-to-next)
    ("[(control c) (control p)]"
     . def-use-jump-to-prev)
    ("[(control c) (control v)]"
     . def-use-show-info))
  "Key bindings for the def-use mode.  The key specifications must be
in a format accepted by the function `define-key'.  Hint: You might
want to type `M-x describe-function def-use <TAB>' to see the
available commands."
  :type '(repeat (cons :tag "Key Binding"
                       (string :tag "Key")
                       (function :tag "Command")))
  :group 'def-use)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points and Positions

(defun def-use-pos-to-point (pos)
  "Returns the value of point in the current buffer at the position."
  (save-excursion
    (goto-line (def-use-pos-line pos))
    (+ (point) (def-use-pos-col pos))))

(defun def-use-point-to-pos (point)
  "Returns the position corresponding to the specified point in the
current buffer."
  (save-excursion
    (goto-char point)
    (def-use-pos
      (+ (count-lines 1 (point))
         (if (= (current-column) 0) 1 0))
      (current-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level symbol lookup

(defun def-use-ref-at-point (point)
  "Returns a reference for the symbol at the specified point in the
current buffer."
  (def-use-ref (def-use-buffer-true-file-name)
    (def-use-point-to-pos
      (save-excursion
        (goto-char point)
        (skip-syntax-backward "w." (def-use-point-at-current-line))
        (point)))))

(defun def-use-sym-at-point (point)
  "Returns symbol information for the symbol at the specified point."
  ;; XXX If data unvailable for current buffer then attempt to load it.
  (def-use-sym-at-ref (def-use-ref-at-point point)))

(defun def-use-current-sym ()
  "Returns symbol information for the symbol at the current point."
  (def-use-sym-at-point (point)))

(defun def-use-current-ref ()
  "Returns a reference to the symbol at the current point."
  (def-use-ref-at-point (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun def-use-jump-to-def (&optional other-window)
  "Jumps to the definition of the symbol under the cursor."
  (interactive "P")
  (let ((sym (def-use-current-sym)))
    (if sym
        (def-use-goto-ref (def-use-sym-ref sym) other-window)
      (message "Sorry, no known symbol at cursor."))))

(defun def-use-jump-to-next (&optional other-window reverse)
  "Jumps to the next use (or def) of the symbol under the cursor."
  (interactive "P")
  (let* ((ref (def-use-current-ref))
         (sym (def-use-sym-at-ref ref)))
    (if (not sym)
        (message "Sorry, no information on the symbol at point!")
      (let* ((uses (def-use-sym-to-uses sym))
             (uses (if reverse (reverse uses) uses))
             (uses (append uses uses)))
        (while (not (equal (pop uses) ref)))
        (def-use-goto-ref (car uses) other-window)))))

(defun def-use-jump-to-prev (&optional other-window)
  "Jumps to the prev use (or def) of the symbol under the cursor."
  (interactive "P")
  (def-use-jump-to-next other-window t))

(defun def-use-goto-ref (ref &optional other-window)
  "Find the referenced source and moves point to the referenced position."
  (cond
   (other-window
    (find-file-other-window (def-use-ref-src ref)))
   ((not (equal (def-use-buffer-true-file-name) (def-use-ref-src ref)))
    (find-file (def-use-ref-src ref))))
  (def-use-goto-pos (def-use-ref-pos ref)))

(defun def-use-goto-pos (pos)
  "Moves point to the specified position."
  (goto-char (def-use-pos-to-point pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info

(defun def-use-show-info ()
  "Shows info on the symbol under the cursor."
  (interactive)
  (let ((sym (def-use-current-sym)))
    (if (not sym)
        (message "Sorry, no information on the symbol at point!")
      (message (def-use-format-sym sym)))))

(defun def-use-format-sym (sym)
  "Formats a string with some basic info on the symbol."
  (format "%s:%d.%d: %s %s, %d uses."
          (def-use-ref-src (def-use-sym-ref sym))
          (def-use-pos-line (def-use-ref-pos (def-use-sym-ref sym)))
          (def-use-pos-col (def-use-ref-pos (def-use-sym-ref sym)))
          (def-use-sym-kind sym)
          (def-use-sym-name sym)
          (length (def-use-sym-to-uses sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting

(defvar def-use-highlighted-sym nil)
(defvar def-use-highlighted-buffer nil)
(defvar def-use-highlighted-overlays nil)

(defun def-use-delete-highlighting ()
  (mapc (function delete-overlay) def-use-highlighted-overlays)
  (setq def-use-highlighted-overlays nil)
  (setq def-use-highlighted-buffer nil)
  (setq def-use-highlighted-sym nil))

(defun def-use-highlight-ref (sym ref face-attr)
  ;; XXX Apply highlight to all open buffers
  (when (equal (def-use-ref-src ref) (def-use-buffer-true-file-name))
    (let* ((begin (def-use-pos-to-point (def-use-ref-pos ref)))
           (beyond (+ begin (length (def-use-sym-name sym))))
           (overlay (make-overlay begin beyond)))
      (push overlay def-use-highlighted-overlays)
      (overlay-put overlay 'priority def-use-priority)
      (overlay-put overlay 'face face-attr))))

(defun def-use-highlight-sym (sym)
  "Highlights the specified symbol."
  (unless (and (equal def-use-highlighted-sym sym)
               (equal def-use-highlighted-buffer (current-buffer)))
    (def-use-delete-highlighting)
    (when sym
      (setq def-use-highlighted-sym sym)
      (setq def-use-highlighted-buffer (current-buffer))
      (def-use-highlight-ref sym (def-use-sym-ref sym) 'def-use-def-face)
      (maphash (function
                (lambda (ref _)
                  (def-use-highlight-ref sym ref 'def-use-use-face)))
               (def-use-sym-to-use-set sym)))))

(defun def-use-highlight-current ()
  "Highlights the symbol at the point."
  (interactive)
  (def-use-highlight-sym (def-use-current-sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting timer

(defvar def-use-highlight-timer nil)

(defun def-use-delete-highlight-timer ()
  (when def-use-highlight-timer
    (def-use-delete-idle-timer def-use-highlight-timer)
    (setq def-use-highlight-timer nil)))

(defun def-use-create-highlight-timer ()
  (unless def-use-highlight-timer
    (setq def-use-highlight-timer
          (run-with-idle-timer
           def-use-delay t
           'def-use-highlight-current))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun def-use-mode-enabled-in-some-buffer ()
  (memq t (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      def-use-mode))
                  (buffer-list))))

(define-minor-mode def-use-mode
  "Minor mode for highlighting and navigating definitions and uses."
  ;; value
  nil
  ;; lighter
  " DU"
  ;; keymap
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result (read (car key-command)) (cdr key-command))))
          def-use-key-bindings)
    result)
  :group 'def-use
  :global t
  (def-use-delete-highlight-timer)
  (def-use-delete-highlighting)
  (when (def-use-mode-enabled-in-some-buffer)
    (def-use-create-highlight-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-mode)
