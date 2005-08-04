;; Emacs mode for editing ML Basis files.
;; Copyright (C) 2005  Vesa Karvonen (vesa.karvonen@cs.helsinki.fi)
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(provide 'esml-mlb-mode)
(require 'cl)

;; Installation
;; ============
;;
;; - Push the path to this file to `load-path' and either
;;     (require 'esml-mlb-mode)
;;   or
;;     (autoload 'esml-mlb-mode "esml-mlb-mode")
;;     (add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))
;;   in your Emacs initialization file.
;;
;;   Alternatively you could use `load-file'.
;;
;;   Beware that (at least) Tuareg mode may already be associated to .mlb
;;   files. You need to add ML Basis mode to `auto-mode-alist' after
;;   Tuareg mode.
;;
;; - Check the `esml-mlb' customization group.
;;
;; Ideas for future development
;; ============================
;;
;; - customisable indentation
;; - movement
;; - completition of path names and variables
;; - type-check / show-basis / compile / compile-and-run
;; - open-file / open-structure / open-signature / open-functor
;; - highlight only binding occurances of basid's
;; - goto-binding-occurance (of a basid)
;; - support doc strings in mlb files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))

(defun esml-mlb-build-font-lock-table ()
  "This is a dummy setter for load-time.") ;; TBD: Is this the best way?

(defun esml-mlb-set-custom-and-build-font-lock-table (sym val)
  "Sets customization variable `sym' to `val' and then builds the font
lock table."
  (custom-set-default sym val)
  (esml-mlb-build-font-lock-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup esml-mlb nil
  "Major mode for editing ML Basis files."
  :group 'sml)

(defcustom esml-mlb-annotations
  '(("allowExport" "false" "true")
    ("allowFFI" "false" "true")
    ("allowImport" "false" "true")
    ("allowOverload" "false" "true")
    ("allowSymbol" "false" "true")
    ("deadCode" "false" "true")
    ("nonexhaustiveExnMatch" "default" "ignore")
    ("nonexhaustiveMatch" "warn" "error" "ignore")
    ("forceUsed")
    ("redundantMatch" "warn" "error" "ignore")
    ("sequenceUnit" "false" "true")
    ("warnMatch" "true" "false")
    ("warnUnused" "false" "true"))
  "Annotations accepted by your compiler(s)."
  :type '(repeat (cons string (repeat string)))
  :set 'esml-mlb-set-custom-and-build-font-lock-table
  :group 'esml-mlb)

(defcustom esml-mlb-indent-offset 3
  "Indentation offset."
  :type 'integer
  :group 'esml-mlb)

(defcustom esml-mlb-mlb-path-map-files
  '("~/.mlton/mlb-path-map"
    "/usr/lib/mlton/mlb-path-map")
  "Files to search for definitions of path variables."
  :type '(repeat file)
  :set 'esml-mlb-set-custom-and-build-font-lock-table
  :group 'esml-mlb)

(defcustom esml-mlb-path-suffix-regexp "fun\\|mlb\\|sig\\|sml"
  "Regexp for matching valid path name suffices."
  :type 'regexp
  :set 'esml-mlb-set-custom-and-build-font-lock-table
  :group 'esml-mlb)

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and highlighting

(defconst esml-mlb-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (function
           (lambda (char-flags)
             (modify-syntax-entry (car char-flags) (cdr char-flags)
                                  table)))
          '((?\( . "$ 1")
            (?\* . ". 23")
            (?\) . "$ 4")
            (?\" . "$")
            (?\\ . "\\")
            (?/  . "_")
            (?-  . "_")
            (?_  . "_")
            (?.  . "_")
            (?\; . ".")
            (?$  . ".")
            (?=  . ".")))
    table)
  "Syntax table for ML Basis mode.")

(defconst esml-mlb-keywords
  '("and" "ann" "bas" "basis" "end" "functor" "in" "let" "local" "open"
    "signature" "structure")
  "Keywords of ML Basis syntax.")

(defun esml-mlb-build-font-lock-table ()
  "Builds the font-lock table for ML Basis mode. Unrecognized
- annotations (see `esml-mlb-annotations'),
- path variables (see `esml-mlb-mlb-path-map-files'), and
- path name suffices (see `esml-mlb-path-suffix-regexp') are
highlighed as warnings."
  (setq esml-mlb-font-lock-table
        `(;; annotations
          (,(apply
             'concat
             "\"[ \t\n]*\\("
             (reduce
              (function
               (lambda (regexps name-values)
                 (if (cdr regexps)
                     (push "\\|" regexps))
                 (cons (if (cdr name-values)
                           (concat (car name-values) "[ \t\n]+\\("
                                   (regexp-opt (cdr name-values)) "\\)")
                         (car name-values))
                       regexps)))
              esml-mlb-annotations
              :initial-value '("\\)[ \t\n]*\"")))
           . font-lock-string-face)
          ("\"[^\"]*\""
           . font-lock-warning-face)
          ;; path variables
          (,(concat
             "\\$(\\("
             (regexp-opt
              (let ((vars nil)
                    (files esml-mlb-mlb-path-map-files))
                (while files
                  (let ((file (pop files)))
                    (if (file-readable-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (skip-chars-forward " \t\n")
                          (while (not (eobp))
                            (let ((start (point)))
                              (skip-chars-forward "^ \t\n")
                              (push (buffer-substring start (point)) vars)
                              (skip-chars-forward "^\n"))
                            (skip-chars-forward " \t\n"))))))
                vars))
             "\\))")
           . font-lock-reference-face)
          ("\\$([^)]*?)"
           . font-lock-warning-face)
          ;; path names
          (,(concat "[-A-Za-z0-9_/.]*\\.\\(" esml-mlb-path-suffix-regexp "\\)\\>")
           . font-lock-constant-face)
          ("[-A-Za-z0-9_/.]*\\.[-A-Za-z0-9_/.]*"
           . font-lock-warning-face)
          ;; keywords
          (,(concat "\\<\\(" (regexp-opt esml-mlb-keywords) "\\)\\>")
           . font-lock-keyword-face)
          ;; variables
          ("[A-Za-z][A-Za-z0-9_']*"
           . font-lock-interface-def-face))))

;; We are now ready to actually build the font-lock table.
(esml-mlb-build-font-lock-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defconst esml-mlb-indent-sync-keywords-regexp
  (concat "\\("
          (regexp-opt '("ann" "bas" "basis" "end" "functor" "in" "let"
                        "local" "open" "signature" "structure"))
          "\\)[ \t\n]")
  "Regexp for synchronizing indentation.")

(defun esml-mlb-previous-indentation ()
  "Finds the previous indentation level and evidence."
  (let ((result nil))
    (save-excursion
      (beginning-of-line)
      (while (not (or (consp result) (bobp)))
        (forward-line -1)
        (beginning-of-line)
        (skip-chars-forward " \t;")
        (cond ((looking-at esml-mlb-indent-sync-keywords-regexp)
               (setq result (let ((start (point))
                                  (indentation (current-column)))
                              (forward-word 1)
                              (cons indentation
                                    (intern (buffer-substring
                                             start
                                             (point)))))))
              ((looking-at "(\\*")
               (setq result (cons (current-column) '*)))
              (t
               (setq result (if result
                                (min result (current-indentation))
                              (current-indentation)))))))
    (cond ((consp result)
           result)
          ((numberp result)
           (cons result 'min))
          (t
           '(0 . min)))))

(defun esml-mlb-indent-line ()
  "Indent current line as ML Basis code."
  (let* ((indent-evidence (esml-mlb-previous-indentation))
         (indent (car indent-evidence))
         (evidence (cdr indent-evidence)))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond ((looking-at ";")
           (case evidence
             ((in)
              (indent-line-to (max 0 (+ indent -2 esml-mlb-indent-offset))))
             (t
              (indent-line-to (max 0 (- indent 2))))))
          ((looking-at "end[ \t\n]")
           (case evidence
             ((ann bas in let local)
              (indent-line-to indent))
             (t
              (indent-line-to (max 0 (- indent esml-mlb-indent-offset))))))
          ((looking-at "in[ \t\n]")
           (case evidence
             ((ann let local)
              (indent-line-to indent))
             (t
              (indent-line-to (- indent esml-mlb-indent-offset)))))
          ((looking-at "and[ \t\n]")
           (case evidence
             ((basis functor signature structure)
              (indent-line-to (+ indent -3 (length (symbol-name evidence)))))
             (t
              (indent-line-to indent))))
          ((looking-at "\\*")
           (case evidence
             ((*)
              (indent-line-to (+ indent 1)))
             (t
              (indent-line-to indent))))
          (t
           (case evidence
             ((ann bas in let local)
              (indent-line-to (+ indent esml-mlb-indent-offset)))
             (t
              (indent-line-to indent)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define mode

(defvar esml-mlb-mode-hook nil
  "Hook run when entering ML Basis mode.")

(defvar esml-mlb-mode-map
  (let ((esml-mlb-mode-map (make-keymap)))
    ;;(define-key wpdl-mode-map "\C-j" 'newline-and-indent)
    esml-mlb-mode-map)
  "Keymap for ML Basis mode.")

(define-derived-mode esml-mlb-mode fundamental-mode "MLB"
  "Major mode for editing ML Basis files."
  :group 'esml-mlb
  (set (make-local-variable 'font-lock-defaults)
       '(esml-mlb-font-lock-table))
  (set (make-local-variable 'indent-line-function)
       'esml-mlb-indent-line))
