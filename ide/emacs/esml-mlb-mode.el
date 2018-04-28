;; Copyright (C) 2005-2007 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'esml-util)

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
;; - type-check / compile / compile-and-run
;; - find-structure / find-signature / find-functor
;; - highlight only binding occurances of basids
;; - find-binding-occurance (of a basid)
;; - support doc strings in mlb files

;; TBD:
;; - fix indentation bugs
;; - use something more robust than `shell-command' to run shell commands

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
  "Major mode for editing ML Basis files."
  :group 'sml)

(defcustom esml-mlb-additional-annotations
  '()
  "Additional annotations accepted by your compiler(s).  Note that ML
Basis mode runs the `esml-mlb-show-annotations-command' to query available
annotations automatically."
  :type '(repeat (cons :tag "Annotation"
                       (string :tag "Name")
                       (repeat :tag "Values starting with the default"
                               string)))
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-additional-path-variables
  '()
  "Additional path variables that can not be found in the path map files
specified by `esml-mlb-mlb-path-map-files' or by running the command
`esml-mlb-show-path-map-command'."
  :type '(repeat (cons (string :tag "Name") (string :tag "Value")))
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-completion-ignored-files-regexp "\\.[^.].*\\|CVS/\\|.*~"
  "Completion ignores files (and directories) whose names match this
regexp."
  :type 'regexp
  :group 'esml-mlb)

(defcustom esml-mlb-indentation-offset 3
  "Basic offset for indentation."
  :type 'integer
  :group 'esml-mlb)

(defcustom esml-mlb-key-bindings
  '(("[tab]"
     . esml-mlb-indent-line-or-complete)
    ("[(control c) (control f)]"
     . esml-mlb-find-file-at-point)
    ("[(control c) (control s)]"
     . esml-mlb-show-basis))
  "Key bindings for the ML Basis mode. The key specifications must be in a
format accepted by the function `define-key'. Hint: You might want to type
`M-x describe-function esml-mlb <TAB>' to see the available commands."
  :type '(repeat (cons :tag "Key Binding"
                       (string :tag "Key")
                       (function :tag "Command")))
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
  "mlton -expert true -show anns"
  "Shell command used to query the available annotations."
  :type 'string
  :set 'esml-mlb-set-custom-and-update
  :group 'esml-mlb)

(defcustom esml-mlb-show-basis-command
  "mlton -stop tc -show-basis %t %f"
  "Shell command used to pretty print the basis defined by an MLB file.
`%t' is replaced by the name of a temporary file and `%f' is replaced by
the name of the MLB file."
  :type 'string
  :group 'esml-mlb)

(defcustom esml-mlb-show-path-map-command
  "mlton -expert true -show path-map"
  "Shell command used to query the available path variables."
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
         (sort (append
                esml-mlb-additional-annotations
                (when (not (string= "" esml-mlb-show-annotations-command))
                  (mapcar (function
                           (lambda (s)
                             (esml-split-string s "[ \t]*[{}|][ \t]*")))
                          (esml-split-string
                           (with-temp-buffer
                             (save-window-excursion
                               (shell-command
                                esml-mlb-show-annotations-command
                                (current-buffer))
                               (buffer-string)))
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

(defun esml-mlb-parse-path-variables-from-string (path-map-string)
  (mapcar (function
           (lambda (s) (apply (function cons) (esml-split-string s "[ \t]+"))))
          (esml-split-string path-map-string "[ \t]*\n+[ \t]*")))

(defun esml-mlb-parse-path-variables ()
  (setq esml-mlb-path-variables
        (remove-duplicates
         (sort (append
                esml-mlb-additional-path-variables
                (esml-mlb-parse-path-variables-from-string
                 (with-temp-buffer
                   (save-window-excursion
                     (shell-command
                      esml-mlb-show-path-map-command
                      (current-buffer))
                     (buffer-string))))
                (loop for file in esml-mlb-mlb-path-map-files
                  append (when (file-readable-p file)
                           (esml-mlb-parse-path-variables-from-string
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))))))
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
                (compat-error "Unknown path variable: %s" name))
              (delete-char (length (match-string 0)))
              (insert (cdr name-value)))
          (forward-char 1)
          (skip-chars-forward "^$")
          (push (buffer-substring 1 (point))
                parts)
          (delete-char (- 1 (point))))
        (goto-char 0)))
    (apply (function concat) (reverse parts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and highlighting

(defconst esml-mlb-string-continue-regexp "\\(?:\\\\[ \t\n]+\\\\\\)")
(defconst esml-mlb-string-char-regexp
  (concat "\\(?:" esml-mlb-string-continue-regexp
          "*\\(?:[^\n\"\\]\\|\\\\[^ \t\n]\\)\\)"))
(defconst esml-mlb-inside-string-regexp
  (concat "\"" esml-mlb-string-char-regexp "*"
          esml-mlb-string-continue-regexp "*"))
(defconst esml-mlb-string-regexp (concat esml-mlb-inside-string-regexp "\""))
(defconst esml-mlb-inside-comment-regexp "(\\*\\(?:[^*]\\|\\*[^)]\\)*")
(defconst esml-mlb-comment-regexp
  (concat esml-mlb-inside-comment-regexp "\\*)"))
(defconst esml-mlb-path-var-chars "A-Za-z0-9_")
(defconst esml-mlb-unquoted-path-chars "-A-Za-z0-9_/.")
(defconst esml-mlb-unquoted-path-or-ref-chars
  (concat esml-mlb-unquoted-path-chars "()$"))
(defconst esml-mlb-compiler-ann-prefix
  (concat "\\(?:" esml-mlb-string-char-regexp "*:[ \t]*\\)"))

(defun esml-mlb-<token>-to-regexp (<token>)
  (let* ((<token>-to-regexp
          '(("<longstrid>" . "[A-Za-z0-9_]*")))
         (<token>-regexp (assoc <token> <token>-to-regexp)))
    (if <token>-regexp
        (cdr <token>-regexp)
      <token>)))

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
          (,(concat esml-mlb-inside-string-regexp
                    "\\.\\(" esml-mlb-path-suffix-regexp "\\)\"")
           . font-lock-constant-face)
          ;; annotations
          (,(apply
             (function concat)
             "\"[ \t]*" esml-mlb-compiler-ann-prefix "?\\("
             (reduce
              (function
               (lambda (regexps name-values)
                 (if (cdr regexps)
                     (push "\\|" regexps))
                 (cons (if (cdr name-values)
                           (concat
                            (car name-values) "[ \t]+\\("
                            (reduce
                             (function
                              (lambda (r s)
                                (concat r "\\|\\("
                                        (esml-mlb-<token>-to-regexp s)
                                        "\\)")))
                             (cddr name-values)
                             :initial-value (concat "\\("
                                                    (esml-mlb-<token>-to-regexp
                                                     (cadr name-values))
                                                    "\\)"))
                            "\\)")
                         (car name-values))
                       regexps)))
              esml-mlb-annotations
              :initial-value '("\\)[ \t]*\"")))
           . font-lock-string-face)
          (,esml-mlb-string-regexp
           . font-lock-warning-face)
          ;; path variables
          (,(concat "\\$(\\("
                    (regexp-opt (mapcar 'car esml-mlb-path-variables))
                    "\\))")
           . font-lock-reference-face)
          ("\\$([^)]*?)"
           . font-lock-warning-face)
          ;; unquoted path names
          (,(concat "[-A-Za-z0-9_/.]*[.]\\("
                    esml-mlb-path-suffix-regexp
                    "\\)[ \t\n]")
           . font-lock-constant-face)
          ("[.][-A-Za-z0-9_]+[ \t\n]"
           . font-lock-warning-face)
          ("[A-Za-z0-9_]*[-/.][-A-Za-z0-9_/]*"
           . font-lock-constant-face)
          ;; keywords
          (,(concat "\\<\\(" (regexp-opt esml-mlb-keywords) "\\)\\>")
           . font-lock-keyword-face)
          ;; basids
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
  (interactive)
  (let* ((indent-evidence (esml-mlb-previous-indentation))
         (indent (car indent-evidence))
         (evidence (cdr indent-evidence)))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond ((looking-at ";")
             (case evidence
               ((in bas)
                (indent-line-to
                 (max 0 (+ indent -2 esml-mlb-indentation-offset))))
               (t
                (indent-line-to (max 0 (- indent 2))))))
            ((looking-at "end[ \t\n]")
             (case evidence
               ((ann bas in let local)
                (indent-line-to indent))
               (t
                (indent-line-to
                 (max 0 (- indent esml-mlb-indentation-offset))))))
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
                               (string-match
                                valid-suffices-regexp ext)))))))))))

(defun esml-mlb-complete ()
  "Performs context sensitive completion at point."
  (interactive)
  (cond
   ;; no completion inside comments
   ((esml-point-preceded-by esml-mlb-inside-comment-regexp))

   ;; annotation values
   ((esml-point-preceded-by
     (concat "\"[ \t\n]*" esml-mlb-compiler-ann-prefix "?\\("
             (regexp-opt (mapcar 'car esml-mlb-annotations))
             "\\)[ \t\n]+\\(" esml-mlb-string-char-regexp "*\\)"))
    (let* ((annot (assoc (match-string 1) esml-mlb-annotations))
           (all-values (cdr annot))
           (values (remove* nil all-values
                            :test (function
                                   (lambda (_ s)
                                     (and (< 0 (length s))
                                          (= ?< (aref s 0)))))))
           (value-prefix (match-string 2))
           (value-completion
            (try-completion value-prefix (mapcar 'list values)))
           (value (if (eq t value-completion) value-prefix value-completion)))
      (message "Annotation: %s %s" (car annot) (if all-values all-values ""))
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
          (concat "\"[ \t\n]*" esml-mlb-compiler-ann-prefix "?\\("
                  esml-mlb-string-char-regexp "*\\)")))
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
           (name-completion
            (try-completion name-prefix esml-mlb-path-variables))
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
         (concat "\\(\"\\)\\(" esml-mlb-string-char-regexp "+\\)"))
        (esml-point-preceded-by
         (concat "\\([ \t\n]\\|^\\)\\(["
                 esml-mlb-unquoted-path-or-ref-chars
                 "]+\\)")))
    ;; TBD: escape sequences in quoted pathnames
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
                        (esml-mlb-filter-file-completions
                         files
                         (esml-string-matches-p "\\(\.\./\\)+" dir))))))
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
            (cond
             ((file-name-directory nondir)
              (message "Completions: %s"
                       (sort (let ((dir (concat dir nondir)))
                               (esml-mlb-filter-file-completions
                                (file-name-all-completions "" dir)
                                (esml-string-matches-p "\\(\.\./\\)+" dir)))
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

(defun esml-mlb-indent-line-or-complete ()
  "Indents the current line. If indentation does not change, attempts to
perform context sensitive completion. This command is not idempotent."
  (interactive)
  (let ((old-indentation (current-indentation)))
    (esml-mlb-indent-line)
    (when (= old-indentation (current-indentation))
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

(defconst esml-mlb-show-basis-process-name "*mlb-show-basis*")

(defun esml-mlb-show-basis ()
  "Shows the basis defined by the MLB file in the current buffer."
  (interactive)
  ;; TBD: find-error / error output mode
  (unless (eq major-mode 'esml-mlb-mode)
    (compat-error "show-basis is only meaningful on MLB files"))
  (when (get-process esml-mlb-show-basis-process-name)
    (compat-error "show-basis already running"))
  (save-some-buffers)
  (lexical-let ((tmp-file (concat
                           (file-name-directory (buffer-file-name))
                           "." (file-name-nondirectory (buffer-file-name))
                           ".basis"))
                (buffer (get-buffer-create esml-mlb-show-basis-process-name)))
    (when (file-exists-p tmp-file)
      (compat-error "Temporary basis file already exists: %s" tmp-file))
    (save-excursion
      (set-buffer buffer)
      (delete-region (point-min) (point-max)))
    (let ((process (start-process-shell-command
                    esml-mlb-show-basis-process-name
                    buffer
                    (compat-replace-regexp-in-string
                     (compat-replace-regexp-in-string
                      esml-mlb-show-basis-command
                      "%t"
                      tmp-file)
                     "%f"
                     (buffer-file-name)))))
      (set-process-sentinel
       process
       (function
        (lambda (process event)
          (if (and (esml-string-matches-p "finished\n" event)
                   (file-readable-p tmp-file))
              (save-excursion
                (set-buffer (find-file-other-window tmp-file))
                (toggle-read-only)
                (delete-file tmp-file))
            (switch-to-buffer buffer))
          (message "%S" event)))))
    (message "%s" "show-basis running...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Map

(defvar esml-mlb-mode-map (make-sparse-keymap)
  "Keymap for ML Basis mode. This variable is updated by
`esml-mlb-update'.")

(defun esml-mlb-build-mode-map ()
  "Builds the key map for ML Basis mode."
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result
               (read (car key-command))
               (cdr key-command))))
          esml-mlb-key-bindings)
    (setq esml-mlb-mode-map result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define mode

(defvar esml-mlb-mode-hook nil
  "Hook run when entering ML Basis mode.")

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
  (esml-mlb-build-font-lock-table)
  (esml-mlb-build-mode-map))

;; We are finally ready to update everything the first time.
(esml-mlb-update)

(add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))
(add-to-list 'auto-mode-alist '("\\.basis\\'" . sml-mode))

(provide 'esml-mlb-mode)
