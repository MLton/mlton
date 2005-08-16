;; Copyright (C) 2005 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(eval-when-compile
  (require 'cl))

;; Emacs mode for editing ML Basis files
;;
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
;; - type-check / show-basis / compile / compile-and-run
;; - open-structure / open-signature / open-functor
;; - highlight only binding occurances of basid's
;; - goto-binding-occurance (of a basid)
;; - support doc strings in mlb files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun esml-point-preceded-by (regexp)
  "Determines whether point is immediately preceded by the given regexp.
If the result is non-nil, the regexp match data will contain the
corresponding match. As with `re-search-backward' the beginning of the
match is as close to the starting point as possible. The end of the match
is always the same as the starting point."
  (save-excursion
    (let ((limit (point))
          (start (re-search-backward regexp 0 t)))
      (when start
        (re-search-forward regexp limit t)
        (= limit (match-end 0))))))

(defun esml-insert-or-skip-if-looking-at (str)
  "Inserts the specified string unless it already follows the point. The
point is moved to the end of the string."
  (if (string= str
               (buffer-substring (point)
                                 (min (+ (point) (length str))
                                      (point-max))))
      (forward-char (length str))
    (insert str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

(defvar esml-mlb-load-time t)

(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))

(defun esml-mlb-set-custom-and-update (sym val)
  (custom-set-default sym val)
  (unless esml-mlb-load-time
    (esml-mlb-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup esml-mlb nil
  "Major mode for editing ML Basis files.

Unrecognized
- annotations (see `esml-mlb-annotations'),
- path variables (see `esml-mlb-mlb-path-map-files'), and
- path name suffices (see `esml-mlb-path-suffix-regexp') are
highlighed as warnings."
  :group 'sml)

(defcustom esml-mlb-additional-path-variables
  '(("LIB_MLTON_DIR" . "/usr/lib/mlton"))
  "Additional path variables that can not be found in the path map files
specified by `esml-mlb-mlb-path-map-files'."
  :type '(repeat (cons (string :tag "Name") (string :tag "Value")))
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-allow-completion t
  "Allow tab-completion if non-nil."
  :type 'boolean
  :group 'esml-mlb)

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
    ("sequenceNonUnit" "ignore" "warn" "error")
    ("sequenceUnit" "false" "true")
    ("warnMatch" "true" "false")
    ("warnUnused" "false" "true"))
  "Annotations accepted by your compiler(s)."
  :type '(repeat (cons :tag "Annotation"
                       (string :tag "Name")
                       (repeat :tag "Values starting with the default"
                               string)))
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-indentation-offset 3
  "Basic offset for indentation."
  :type 'integer
  :group 'esml-mlb)

(defcustom esml-mlb-mlb-path-map-files
  '("~/.mlton/mlb-path-map"
    "/usr/lib/mlton/mlb-path-map")
  "Files to search for definitions of path variables."
  :type '(repeat file)
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-path-suffix-regexp "fun\\|mlb\\|sig\\|sml"
  "Regexp for matching valid path name suffices."
  :type 'regexp
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path variables

(defvar esml-mlb-path-variables nil
  "An association list of known path variables. This variable is updated
by `esml-mlb-update'.")

(defun esml-mlb-parse-path-variables ()
  (setq esml-mlb-path-variables nil)
  (loop for file in esml-mlb-mlb-path-map-files
    do (if (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (skip-chars-forward " \t\n")
             (while (not (eobp))
               (push (cons (let ((start (point)))
                             (skip-chars-forward "^ \t\n")
                             (buffer-substring start (point)))
                           (progn
                             (skip-chars-forward " \t")
                             (let ((start (point)))
                               (skip-chars-forward "^ \t\n")
                               (buffer-substring start (point)))))
                     esml-mlb-path-variables)
               (skip-chars-forward " \t\n")))))
  (setq esml-mlb-path-variables
        (sort (append esml-mlb-additional-path-variables
                      esml-mlb-path-variables)
              (function (lambda (a b)
                          (string-lessp (car a) (car b)))))))

(defun esml-mlb-expand-path (path)
  "Expands path variable references in the given path."
  (let ((parts nil))
    (with-temp-buffer
      (insert path)
      (goto-char 0)
      (while (not (eobp))
        (if (looking-at "\\$(\\([^)]+\\))")
            (let* ((name (match-string 1))
                   (name-value (assoc name esml-mlb-path-variables)))
              (unless name-value
                (error 'invalid-argument name))
              (delete-char (length (match-string 0)))
              (insert (cdr name-value)))
          (forward-char 1)
          (skip-chars-forward "[^$]")
          (push (buffer-substring 1 (point))
                parts)
          (delete-char (- 1 (point))))
        (goto-char 0)))
    (apply 'concat (reverse parts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and highlighting

(defconst esml-mlb-str-chr-regexp "\\([^\n\"\\]\\|\\\\.\\)")
(defconst esml-mlb-string-regexp (concat "\"" esml-mlb-str-chr-regexp "+\""))
(defconst esml-mlb-comment-regexp "(\\*\\([^*]\\|\\*[^)]\\)*\\*)")
(defconst esml-mlb-path-var-chars "A-Za-z0-9_")
(defconst esml-mlb-unquoted-path-chars "-A-Za-z0-9_/.")
(defconst esml-mlb-unquoted-path-or-ref-chars
  (concat esml-mlb-unquoted-path-chars "()$"))

(defconst esml-mlb-keywords
  '("and" "ann" "bas" "basis" "end" "functor" "in" "let" "local" "open"
    "signature" "structure")
  "Keywords of ML Basis syntax.")

(defconst esml-mlb-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (function
           (lambda (char-flags)
             (modify-syntax-entry (car char-flags) (cdr char-flags)
                                  table)))
          '((?\( . "()1")
            (?\* . ". 23")
            (?\) . ")(4")
            (?\" . "$") ;; not '"' to allow custom highlighting of ann
            (?\\ . "/") ;; not '\' due to class of '"'
            (?/  . "_")
            (?-  . "_")
            (?_  . "_")
            (?.  . "_")
            (?$  . "_")
            (?\; . ".")
            (?=  . ".")))
    table)
  "Syntax table for ML Basis mode.")

(defvar esml-mlb-font-lock-table nil)

(defun esml-mlb-build-font-lock-table ()
  "Builds the font-lock table for ML Basis mode."
  (setq esml-mlb-font-lock-table
        `(;; quoted path names
          (,(concat "\"" esml-mlb-str-chr-regexp "*\\.\\(" esml-mlb-path-suffix-regexp "\\)\"")
           . font-lock-constant-face)
          ;; annotations
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
          (,(concat "\"" esml-mlb-str-chr-regexp "*\"")
           . font-lock-warning-face)
          ;; path variables
          (,(concat "\\$(\\(" (regexp-opt (mapcar 'car esml-mlb-path-variables)) "\\))")
           . font-lock-reference-face)
          ("\\$([^)]*?)"
           . font-lock-warning-face)
          ;; unquoted path names
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
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond ((looking-at ";")
             (case evidence
               ((in bas)
                (indent-line-to (max 0 (+ indent -2 esml-mlb-indentation-offset))))
               (t
                (indent-line-to (max 0 (- indent 2))))))
            ((looking-at "end[ \t\n]")
             (case evidence
               ((ann bas in let local)
                (indent-line-to indent))
               (t
                (indent-line-to (max 0 (- indent esml-mlb-indentation-offset))))))
            ((looking-at "in[ \t\n]")
             (case evidence
               ((ann let local)
                (indent-line-to indent))
               (t
                (indent-line-to (- indent esml-mlb-indentation-offset)))))
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
                (indent-line-to (+ indent esml-mlb-indentation-offset)))
               (t
                (indent-line-to indent))))))
    (if (< (current-column) (current-indentation))
        (forward-char (- (current-indentation) (current-column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defun esml-mlb-complete ()
  "Performs context sensitive completion."
  (interactive)
  (cond
   ((esml-point-preceded-by (concat "\"[ \t\n]*\\("
                                    (regexp-opt (mapcar 'car esml-mlb-annotations))
                                    "\\)[ \t\n]+\\(" esml-mlb-str-chr-regexp "*\\)"))
    (let* ((annot (assoc (match-string 1) esml-mlb-annotations))
           (values (cdr annot))
           (value-prefix (match-string 2))
           (value-completion (try-completion value-prefix (mapcar 'list values)))
           (value (if (eq t value-completion) value-prefix value-completion)))
      (message "Annotation: %s %s" (car annot) (if values values ""))
      (when (stringp value-completion)
        (esml-insert-or-skip-if-looking-at
         (substring value (length value-prefix))))
      (when (and value
                 (eq t (try-completion value (mapcar 'list values))))
        (esml-insert-or-skip-if-looking-at "\""))))

   ((and (esml-point-preceded-by
          (concat "\\<ann[ \t\n]+\\([ \t\n]+\\|" esml-mlb-string-regexp
                  "\\|" esml-mlb-comment-regexp "\\)*\"[^\"]*"))
         (esml-point-preceded-by
          (concat "\"[ \t\n]*\\(" esml-mlb-str-chr-regexp "*\\)")))
    (let* ((name-prefix (match-string 1))
           (name-completion (try-completion name-prefix esml-mlb-annotations))
           (name (if (eq t name-completion) name-prefix name-completion)))
      (if (not name-completion)
          (message "Annotations: %s" (mapcar 'car esml-mlb-annotations))
        (when (stringp name-completion)
          (esml-insert-or-skip-if-looking-at
           (substring name (length name-prefix))))
        (if (and name
                 (eq t (try-completion name esml-mlb-annotations)))
            (let ((values (cdr (assoc name esml-mlb-annotations))))
              (esml-insert-or-skip-if-looking-at (if values " " "\""))
              (message "Annotation: %s %s" name (if values values "")))
          (message "Annotations: %s"
                   (all-completions name-prefix esml-mlb-annotations))))))

   ((esml-point-preceded-by (concat "\\$(\\([" esml-mlb-path-var-chars "]*\\)"))
    (let* ((name-prefix (match-string 1))
           (name-completion (try-completion name-prefix esml-mlb-path-variables))
           (name (if (eq t name-completion) name-prefix name-completion)))
      (if (not name-completion)
          (message "Path variables: %s" (mapcar 'car esml-mlb-path-variables))
        (when (stringp name-completion)
          (esml-insert-or-skip-if-looking-at
           (substring name (length name-prefix))))
        (if (and name
                 (eq t (try-completion name esml-mlb-path-variables)))
            (let* ((value (cdr (assoc name esml-mlb-path-variables)))
                   (expanded (esml-mlb-expand-path value)))
              (esml-insert-or-skip-if-looking-at ")")
              (if (string= value expanded)
                  (message "Path variable: %s [%s]" name value)
                (message "Path variable: %s [%s ==> %s]" name value expanded)))
          (message "Path variables: %s"
                   (all-completions name-prefix esml-mlb-path-variables))))))

   ((or (esml-point-preceded-by
         (concat "\\(\"\\)\\(" esml-mlb-str-chr-regexp "+\\)"))
        (esml-point-preceded-by
         (concat "\\([ \t\n]\\)\\([" esml-mlb-unquoted-path-or-ref-chars "]+\\)")))
    (let* ((quoted (string= "\"" (match-string 1)))
           (path-prefix (match-string 2))
           (path-expanded (esml-mlb-expand-path path-prefix))
           (dir (if (file-name-directory path-expanded)
                    (file-name-directory path-expanded)
                  ""))
           (nondir-prefix (file-name-nondirectory path-expanded))
           (nondir-completion (file-name-completion nondir-prefix dir))
           (nondir (if (eq t nondir-completion)
                       nondir-prefix
                     nondir-completion)))
      (if (not nondir-completion)
          (if (string= path-prefix path-expanded)
              (message "No completions for %s" path-prefix)
            (message "No completions for %s ==> %s" path-prefix path-expanded))
        (when (stringp nondir-completion)
          (esml-insert-or-skip-if-looking-at
           (substring nondir-completion (length nondir-prefix))))
        (if (and nondir
                 (eq t (file-name-completion nondir dir)))
            (progn
              (esml-insert-or-skip-if-looking-at (if quoted "\"" ""))
              (message "Expanded path: %s%s" dir nondir))
          (message "File name completions: %s"
                   (if (file-name-directory nondir)
                       (file-name-all-completions "" (concat dir nondir))
                     (file-name-all-completions nondir dir)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun esml-mlb-indent-or-complete ()
  "Indents the current line. If indentation does not change, attempts to
perform context sensitive completion."
  (interactive)
  (let ((old-indentation (current-indentation)))
    (esml-mlb-indent-line)
    (when (and esml-mlb-allow-completion
               (= old-indentation (current-indentation)))
      (esml-mlb-complete))))

(defun esml-mlb-find-file-at-point ()
  "Grabs the path surrounding point and attempts to find the file."
  (interactive)
  (let ((path (save-excursion
                (if (and (not (bobp))
                         (= ?\" (char-before)))
                    (let ((end (point)))
                      (backward-sexp)
                      (buffer-substring (+ (point) 1) (- end 1)))
                  (skip-chars-backward esml-mlb-unquoted-path-or-ref-chars)
                  (let ((start (point)))
                    (skip-chars-forward esml-mlb-unquoted-path-or-ref-chars)
                    (buffer-substring start (point)))))))
    (find-file (esml-mlb-expand-path path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define mode

(defvar esml-mlb-mode-hook nil
  "Hook run when entering ML Basis mode.")

(defvar esml-mlb-mode-map
  (let ((esml-mlb-mode-map (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key esml-mlb-mode-map
               (car key-command) (cdr key-command))))
          '(([tab]
             . esml-mlb-indent-or-complete)
            ([(control c) (control f)]
             . esml-mlb-find-file-at-point)))
    esml-mlb-mode-map)
  "Keymap for ML Basis mode.")

(define-derived-mode esml-mlb-mode fundamental-mode "MLB"
  "Major mode for editing ML Basis files."
  :group 'esml-mlb
  (set (make-local-variable 'font-lock-defaults)
       '(esml-mlb-font-lock-table))
  (set (make-local-variable 'indent-line-function)
       'esml-mlb-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalization

(setq esml-mlb-load-time nil)

(defun esml-mlb-update ()
  "Updates data based on customization variables."
  (interactive)
  ;; Warning: order dependencies
  (esml-mlb-parse-path-variables)
  (esml-mlb-build-font-lock-table))

;; We are finally ready to update everything the first time.
(esml-mlb-update)

(provide 'esml-mlb-mode)
