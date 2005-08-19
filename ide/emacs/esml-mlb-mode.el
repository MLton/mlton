;; Copyright (C) 2005 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(eval-when-compile
  (require 'cl)
  (require 'esml-util))

;; Emacs mode for editing ML Basis files
;;
;; Installation
;; ============
;;
;; - Push the path to this file (and `esml-util.el') to `load-path' and
;;   either
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
;; - find-structure / find-signature / find-functor
;; - highlight only binding occurances of basid's
;; - find-binding-occurance (of a basid)
;; - support doc strings in mlb files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

(defvar esml-mlb-load-time t)

(defun esml-mlb-set-custom-and-update (sym val)
  (custom-set-default sym val)
  (unless esml-mlb-load-time
    (esml-mlb-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup esml-mlb nil
  "Major mode for editing ML Basis files.

Unrecognized
- annotations (see `esml-mlb-show-annotations-command' and
                   `esml-mlb-additional-annotations'),
- path variables (see `esml-mlb-mlb-path-map-files' and
                      `esml-mlb-additional-path-variables'), and
- path name suffices (see `esml-mlb-path-suffix-regexp') are
highlighed as warnings."
  :group 'sml)

(defcustom esml-mlb-additional-annotations
  '(("allowConstant" "false" "true")
    ("allowFFI" "false" "true")
    ("allowOverload" "false" "true")
    ("allowPrim" "false" "true")
    ("allowRebindEquals" "false" "true")
    ("deadCode" "false" "true")
    ("ffiStr" "[A-Za-z0-9_]*")
    ("forceUsed")
    ("nonexhaustiveExnMatch" "default" "ignore")
    ("nonexhaustiveMatch" "warn" "ignore" "error")
    ("redundantMatch" "warn" "ignore" "error")
    ("sequenceNonUnit" "ignore" "error" "warn")
    ("warnUnused" "false" "true"))
  "Additional annotations accepted by your compiler(s)."
  :type '(repeat (cons :tag "Annotation"
                       (string :tag "Name")
                       (repeat :tag "Values starting with the default"
                               regexp)))
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

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

(defcustom esml-mlb-completion-ignored-files-regexp "\\.[^.].*\\|CVS/"
  "Completion ignores files (and directories) whose names match this
regexp."
  :type 'regexp
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
  "Regexp for matching valid path name suffices. Completion only considers
files whose extension matches this regexp."
  :type 'regexp
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-show-annotations-command
  "mlton -expert true -show-anns true"
  "Command used to determine the annotations accepted by a compiler."
  :type 'string
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(defvar esml-mlb-annotations nil
  "An association list of known annotations. This variable is updated by
`esml-mlb-update'.")

(defun esml-mlb-parse-annotations ()
  (setq esml-mlb-annotations
        (remove-duplicates
         (sort (append esml-mlb-additional-annotations
                       (when (not (string= "" esml-mlb-show-annotations-command))
                         (mapcar (function
                                  (lambda (s)
                                    (esml-split-string s "[ \t]*[{}|][ \t]*")))
                                 (esml-split-string
                                  (with-temp-buffer
                                    (if (zerop
                                         (condition-case nil
                                             (let ((cmd-and-args
                                                    (esml-split-string
                                                     esml-mlb-show-annotations-command
                                                     " +")))
                                               (apply 'call-process
                                                      (car cmd-and-args) nil t nil (cdr cmd-and-args)))
                                           (error -1)))
                                        (esml-replace-regexp-in-string
                                         (buffer-string)
                                         "{[ \t]*None[ \t]*|[ \t]*Some[ \t]*<[^>]+>}"
                                         "{[A-Za-z0-9_]*}")
                                      (message "Show annotations command failed.")
                                      ""))
                                  "[ \t]*\n+[ \t]*"))))
               (function
                (lambda (a b)
                  (string-lessp (car a) (car b)))))
         :test (function
                (lambda (a b)
                  (string= (car a) (car b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path variables

(defvar esml-mlb-path-variables nil
  "An association list of known path variables. This variable is updated
by `esml-mlb-update'.")

(defun esml-mlb-parse-path-variables ()
  (setq esml-mlb-path-variables
        (remove-duplicates
         (sort (append esml-mlb-additional-path-variables
                       (loop for file in esml-mlb-mlb-path-map-files
                         append (mapcar (function
                                         (lambda (s)
                                           (apply 'cons
                                                  (esml-split-string s "[ \t]+"))))
                                        (esml-split-string (with-temp-buffer
                                                             (insert-file-contents file)
                                                             (buffer-string))
                                                           "[ \t]*\n+[ \t]*"))))
               (function
                (lambda (a b)
                  (string-lessp (car a) (car b)))))
         :test (function
                (lambda (a b)
                  (string= (car a) (car b)))))))

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
(defconst esml-mlb-inside-comment-regexp "(\\*\\([^*]\\|\\*[^)]\\)*")
(defconst esml-mlb-comment-regexp (concat esml-mlb-inside-comment-regexp "\\*)"))
(defconst esml-mlb-path-var-chars "A-Za-z0-9_")
(defconst esml-mlb-unquoted-path-chars "-A-Za-z0-9_/.")
(defconst esml-mlb-unquoted-path-or-ref-chars
  (concat esml-mlb-unquoted-path-chars "()$"))

(defconst esml-mlb-keywords
  '("and" "ann" "bas" "basis" "end" "functor" "in" "let" "local" "open"
    "signature" "structure")
  "Keywords of ML Basis syntax.")

(defconst esml-mlb-keywords-usually-followed-by-space
  '("and" "functor" "open" "signature" "structure")
  "Keywords of ML Basis syntax that are under most circumstances followed
by a space.")

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
            (?_  . "w") ;; not "_" due to variables regexp
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
                                   (reduce (function
                                            (lambda (r s)
                                              (concat r "\\|\\(" s "\\)")))
                                           (cddr name-values)
                                           :initial-value (concat "\\(" (cadr name-values) "\\)"))
                                   "\\)")
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

(defun esml-mlb-filter-file-completions (completions &optional allow-dots)
  "Removes the directories `./' and `../' as well as files whose suffix
does not appear in `esml-mlb-path-suffix-regexp' from the list of file
name completions."
  (let ((ignored-files-regexp
         (concat "^\\(" esml-mlb-completion-ignored-files-regexp "\\)$"))
        (valid-suffices-regexp
         (concat "^\\(" esml-mlb-path-suffix-regexp "\\)$")))
    (remove*
     nil
     completions
     :test (function
            (lambda (_ x)
              (or (and (not allow-dots)
                       (member x '("./" "../")))
                  (string-match ignored-files-regexp x)
                  (not (or (file-name-directory x)
                           (let ((ext (file-name-extension x)))
                             (when ext
                               (string-match valid-suffices-regexp ext)))))))))))

(defun esml-mlb-complete ()
  "Performs context sensitive completion."
  (interactive)
  (cond
   ;; no completion inside comments
   ((esml-point-preceded-by esml-mlb-inside-comment-regexp))

   ;; annotation values
   ((esml-point-preceded-by (concat "\"[ \t\n]*\\("
                                    (regexp-opt (mapcar 'car esml-mlb-annotations))
                                    "\\)[ \t\n]+\\(" esml-mlb-str-chr-regexp "*\\)"))
    ;; TBD: do not auto-complete non-trivial regexps
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

   ;; annotation names
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

   ;; path variables
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

   ;; filenames and keywords
   ((or (esml-point-preceded-by
         (concat "\\(\"\\)\\(" esml-mlb-str-chr-regexp "+\\)"))
        (esml-point-preceded-by
         (concat "\\([ \t\n]\\|^\\)\\([" esml-mlb-unquoted-path-or-ref-chars "]+\\)")))
    ;; TBD: escape sequences in quoted pathnames
    ;; TBD: ../../
    (let* ((quoted (string= "\"" (match-string 1)))
           (path-prefix (match-string 2))
           (path-expanded (esml-mlb-expand-path path-prefix))
           (dir (if (file-name-directory path-expanded)
                    (file-name-directory path-expanded)
                  ""))
           (nondir-prefix (file-name-nondirectory path-expanded))
           (nondir-completions
            (mapcar 'list
                    (let ((files (esml-mlb-filter-file-completions
                                  (file-name-all-completions nondir-prefix dir)
                                  t)))
                      (if (string= "" dir)
                          (if quoted
                              files
                            (append (all-completions
                                     nondir-prefix
                                     (mapcar 'list esml-mlb-keywords))
                                    files))
                        (esml-mlb-filter-file-completions files)))))
           (nondir-completion (try-completion nondir-prefix nondir-completions))
           (nondir (if (eq t nondir-completion)
                       nondir-prefix
                     nondir-completion)))
      (if (not nondir-completion)
          (if (string= path-prefix path-expanded)
              (message "No completions for %s" path-prefix)
            (message "No completions for %s ==> %s" path-prefix path-expanded))
        (when (stringp nondir-completion)
          (esml-insert-or-skip-if-looking-at
           (substring nondir (length nondir-prefix))))
        (if (eq t (try-completion nondir nondir-completions))
            (cond ((file-name-directory nondir)
                   (message "Completions: %s"
                            (sort (esml-mlb-filter-file-completions
                                   (file-name-all-completions
                                    ""
                                    (concat dir nondir)))
                                  'string-lessp)))
                  ((member nondir esml-mlb-keywords)
                   (esml-mlb-indent-line)
                   (message "Keyword: %s" nondir)
                   (when (member nondir esml-mlb-keywords-usually-followed-by-space)
                     (esml-insert-or-skip-if-looking-at " ")))
                  (t
                   (message "Expanded path: %s%s" dir nondir)))
          (message "Completions: %s"
                   (sort (mapcar 'car nondir-completions)
                         'string-lessp))))))))

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
  (let ((file (esml-mlb-expand-path
               (save-excursion
                 (if (and (not (bobp))
                          (= ?\" (char-before)))
                     (let ((end (point)))
                       (backward-sexp)
                       (buffer-substring (+ (point) 1) (- end 1)))
                   (skip-chars-backward esml-mlb-unquoted-path-or-ref-chars)
                   (let ((start (point)))
                     (skip-chars-forward esml-mlb-unquoted-path-or-ref-chars)
                     (buffer-substring start (point))))))))
    (if (file-readable-p file)
        (find-file file)
      (message (if (file-exists-p file)
                   "Not readable: %s"
                 "Does not exists: %s")
               file))))

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
  "Major mode for editing ML Basis files. Provides syntax highlighting,
indentation, and context sensitive completion.

See the customization group `esml-mlb'."
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
  (esml-mlb-parse-annotations)
  (esml-mlb-build-font-lock-table))

;; We are finally ready to update everything the first time.
(esml-mlb-update)

(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))

(provide 'esml-mlb-mode)
