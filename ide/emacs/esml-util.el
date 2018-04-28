;; Copyright (C) 2005 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'cl)
(require 'compat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SML metadata

(defconst esml-sml-symbolic-chars "-!%&$#+/:<=>?@~`^|*\\\\"
  "A string of all Standard ML symbolic characters as defined in section
2.4 of the Definition.")

(defconst esml-sml-alphanumeric-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'_"
  "A string of all Standard ML alphanumeric characters as defined in
section 2.4 of the Definition.")

(defconst esml-sml-symbolic-keywords '("#" "*" "->" ":" "::" ":>" "=" "=>" "|")
  "A list of symbolic keywords or reserved words as defined in sections
2.1 and section 3.1 and including the special symbol * mentioned in 2.4 as
well as the symbol :: mentioned in section 2.9 of the Definition.")

(defconst esml-sml-alphanumeric-keywords
  '("_" "abstype" "and" "andalso" "as" "case" "datatype" "do" "else" "end"
    "eqtype" "exception" "false" "fn" "fun" "functor" "handle" "if" "in"
    "include" "infix" "infixr" "let" "local" "nil" "nonfix" "of" "op" "open"
    "orelse" "raise" "rec" "ref" "sharing" "sig" "signature" "struct"
    "structure" "then" "true" "type" "val" "where" "while" "with" "withtype")
  "A list of alphanumeric keywords or reserved words as well as
non-bindable identifiers defined in various sections of the Definition")

(defconst esml-sml-numeric-literal-regexp
  "\\(?:\\(?:0w\\)?[0-9]+\\|0w?x[0-9a-fA-F]+\\)"
  "Regexp matching the syntax of Standard ML numeric literals.")

(defconst esml-sml-modes '(sml-mode sml-lex-mode sml-yacc-mode)
  "List of Emacs modes dealing with SML code.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some general purpose Emacs Lisp utility functions

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

(defun esml-split-string (string separator)
  (remove* "" (split-string string separator) :test 'equal))

(defun esml-string-matches-p (regexp str)
  "Non-nil iff the entire string matches the regexp."
  (and (string-match regexp str)
       (= 0 (match-beginning 0))
       (= (length str) (match-end 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-util)
