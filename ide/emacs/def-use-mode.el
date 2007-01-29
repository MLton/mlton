;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

;; TBD:
;; - jump-to-next
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

(defun def-use-sym-at-point (point)
  "Returns symbol information for the symbol at the specified point."
  ;; XXX If data unvailable for current buffer then attempt to load it.
  (let ((pos
         (def-use-point-to-pos
           (save-excursion
             (goto-char point)
             (skip-syntax-backward "w." (def-use-point-at-current-line))
             (point)))))
    (def-use-sym-at-ref (def-use-ref (def-use-buffer-true-file-name) pos))))

(defun def-use-current-sym ()
  "Returns symbol information for the symbol at the current point."
  (def-use-sym-at-point (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun def-use-jump-to-def ()
  "Jumps to the definition of the symbol under the cursor."
  (interactive)
  (let ((sym (def-use-current-sym)))
    (if sym
        (def-use-goto-ref (def-use-sym-ref sym))
      (message "Sorry, no known symbol at cursor."))))

(defun def-use-goto-ref (ref)
  "Find the referenced source and moves point to the referenced position."
  (find-file (def-use-ref-src ref))
  (def-use-goto-pos (def-use-ref-pos ref)))

(defun def-use-goto-pos (pos)
  "Moves point to the specified position."
  (goto-char (def-use-pos-to-point pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting

(defvar def-use-highlighted-sym nil)
(defvar def-use-highlighted-overlays nil)

(defun def-use-delete-highlighting ()
  (mapc (function delete-overlay) def-use-highlighted-overlays)
  (setq def-use-highlighted-overlays nil)
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
  (unless (equal sym def-use-highlighted-sym)
    (def-use-delete-highlighting)
    (when sym
      (setq def-use-highlighted-sym sym)
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
  "Toggless the def-use highlighting mode."
  :group 'def-use
  :global t
  :lighter " DU"
  (def-use-delete-highlight-timer)
  (def-use-delete-highlighting)
  (when (def-use-mode-enabled-in-some-buffer)
    (def-use-create-highlight-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-mode)
