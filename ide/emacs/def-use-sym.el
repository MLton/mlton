;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'def-use-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points and Positions

(defun def-use-pos-to-point (pos)
  "Returns the value of point in the current buffer at the position."
  (save-excursion
    (def-use-goto-line (def-use-pos-line pos))
    (+ (point) (def-use-pos-col pos))))

(defun def-use-point-to-pos (point)
  "Returns the position corresponding to the specified point in the
current buffer."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (let ((line (+ (count-lines 1 (point)) 1))
          (col (- point (point))))
      (def-use-pos line col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic symbol lookup support

(defvar def-use-mode-to-move-to-symbol-start-alist nil
  "Association list mapping modes to functions that move the point
backwards to the start of the symbol at the point.")

(defvar def-use-mode-to-move-to-symbol-end-alist nil
  "Association list mapping modes to functions that move the point to the
end of the symbol at the point.")

(defun def-use-move-to-symbol-start ()
  (let ((mode-move
         (assoc major-mode def-use-mode-to-move-to-symbol-start-alist)))
    (if mode-move
        (funcall (cdr mode-move))
      (skip-syntax-backward "w_" (def-use-point-at-current-line)))))

(defun def-use-move-to-symbol-end ()
  (let ((mode-move
         (assoc major-mode def-use-mode-to-move-to-symbol-end-alist)))
    (if mode-move
        (funcall (cdr mode-move))
      (skip-syntax-forward "w_" (def-use-point-at-next-line)))))

(defun def-use-ref-at-point (point)
  "Returns a reference for the symbol at the specified point in the
current buffer."
  (let ((src (def-use-buffer-file-truename)))
    (when src
      (def-use-ref src
        (def-use-point-to-pos
          (save-excursion
            (goto-char point)
            (def-use-move-to-symbol-start)
            (point)))))))

(defun def-use-extract-sym-name-at-point (point)
  "Tries to extracts what looks like the name of the symbol at point.
This doesn't really understand the syntax of the language, so the result
is only valid when there really is a symbol at the point."
  (save-excursion
    (goto-char point)
    (let* ((start (progn (def-use-move-to-symbol-start) (point)))
           (end (progn (def-use-move-to-symbol-end) (point))))
      (when (and (<= start point)
                 (<= point end)
                 (< start end))
        (buffer-substring start end)))))

(defun def-use-extract-sym-name-at-ref (ref)
  "Tries to extract what looks like the name of the symbol at ref."
  (save-window-excursion
    (def-use-find-file (def-use-ref-src ref))
    (def-use-extract-sym-name-at-point
      (def-use-pos-to-point (def-use-ref-pos ref)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-use-sym)
