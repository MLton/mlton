;; Copyright (C) 2005 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'esml-util)

;; Installation
;; ============
;;
;; Push the path to this file (and `esml-util.el') to `load-path' and use
;; (require 'esml-gen).
;;
;; Ideas for future development
;; ============================
;;
;; - ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup esml-gen nil
  "Code generation functions for Standard ML."
  :group 'sml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

;; TBD: Consider moving these to another place if/when it makes sense.

(defun esml-extract-field-names (pattern-or-type)
  (let ((fields nil))
    (with-temp-buffer
      (insert pattern-or-type)
      (goto-char 0)
      (skip-chars-forward " \t\n{},")
      (while (not (eobp))
        (let ((start (point)))
          (if (find (char-after) esml-sml-symbolic-chars)
              (skip-chars-forward esml-sml-symbolic-chars)
            (skip-chars-forward esml-sml-alphanumeric-chars))
          (push (buffer-substring start (point)) fields))
        (skip-chars-forward " \t\n")
        (when (and (not (eobp))
                   (= ?\: (char-after)))
          (let ((open-parens 0))
            (while (not (or (eobp)
                            (and (zerop open-parens)
                                 (= ?\, (char-after)))))
              (cond ((or (= ?\( (char-after))
                         (= ?\{ (char-after)))
                     (setq open-parens (1+ open-parens)))
                    ((or (= ?\) (char-after))
                         (= ?\} (char-after)))
                     (setq open-parens (1- open-parens))))
              (forward-char 1)
              (skip-chars-forward "^,(){}"))))
        (skip-chars-forward " \t\n{},")))
    fields))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Record Update (see http://mlton.org/FunctionalRecordUpdate)

(defcustom esml-gen-fru-setter-template
  '("fun set f =\nlet\nfun t2r (%1) = {%2}\nfun r2t {%3} = (%4)
in\nRecords.wrapSet (Tuples.set%n, t2r, t2r, r2t) f\nend\n"
    "v%i, "
    "%f = v%i, "
    "%f = v%i, "
    "v%i, ")
  "Template for `esml-gen-fru-setter'. Indentation is automatic. The last
two characters of a pattern are deleted at the end."
  :type '(list :tag "Template"
               (string :tag "Code (`%1' = 1., `%2' = 2., ..., `%n' = n)")
               (string :tag "1. pattern (`%i' = index, `%f' = field)")
               (string :tag "2. pattern (`%i' = index, `%f' = field)")
               (string :tag "3. pattern (`%i' = index, `%f' = field)")
               (string :tag "4. pattern (`%i' = index, `%f' = field)"))
  :group 'esml-gen)

(defun esml-gen-fru-setter (pattern-or-type)
  "Generates a functional record update function. The parameter must be in
the format `[{]id[: ty][,] ...[,] id[}]' where `[]' marks optional parts."
  (interactive "sSimple record pattern or type: ")
  (let* ((fields (esml-extract-field-names pattern-or-type))
         (n (length fields)))
    (if (< n 2)
        (compat-error "%s" "Record must have at least two fields")
      (let ((fields (sort fields 'string-lessp))
            (start (point)))
        (labels ((format-fields
                  (fmt)
                  (with-temp-buffer
                    (loop
                      for f in fields
                      for i from 1 to n
                      do (insert
                          (let* ((result fmt)
                                 (result (compat-replace-regexp-in-string
                                          result "\\%f" f))
                                 (result (compat-replace-regexp-in-string
                                          result "\\%i" (int-to-string i))))
                            result)))
                    (delete-char -2) ;; TBD
                    (buffer-string))))
          (insert
           (let* ((result (nth 0 esml-gen-fru-setter-template))
                  (result (compat-replace-regexp-in-string
                           result "%1" (format-fields (nth 1 esml-gen-fru-setter-template))))
                  (result (compat-replace-regexp-in-string
                           result "%2" (format-fields (nth 2 esml-gen-fru-setter-template))))
                  (result (compat-replace-regexp-in-string
                           result "%3" (format-fields (nth 3 esml-gen-fru-setter-template))))
                  (result (compat-replace-regexp-in-string
                           result "%4" (format-fields (nth 4 esml-gen-fru-setter-template))))
                  (result (compat-replace-regexp-in-string
                           result "%n" (int-to-string n))))
             result))
          (indent-region start (point) nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Tuple Update (see http://mlton.org/FunctionalRecordUpdate)

(defcustom esml-gen-ftu-setters-template
  '("fun set%n f v (%1) =\nlet\ndatatype (%2) t =\n%3
fun g h v =\n(%4)\nin\nf (%5) v\nend\n"
    "v%i, "
    "'v%i, "
    " V%i of 'v%i |"
    "case h v of V%i v%i => v%i | _ => v%i,\n"
    "g V%i, ")
  "Template for `esml-gen-ftu-setters'. Indentation is automatic. The last
two characters of a pattern are deleted at the end."
  :type '(list :tag "Format"
               (string :tag "Code (`%1' = 1., `%2' = 2., ..., `%n' = n)")
               (string :tag "1. pattern (`%i' = index)")
               (string :tag "2. pattern (`%i' = index)")
               (string :tag "3. pattern (`%i' = index)")
               (string :tag "4. pattern (`%i' = index)")
               (string :tag "5. pattern (`%i' = index)"))
  :group 'esml-gen)

(defun esml-gen-ftu-setters (n)
  "Generates functional tuple update, or `set<N>', functions."
  (interactive "nMaximum number of fields [2-100]: ")
  (if (not (and (<= 2 n)
                (<= n 100)))
      (compat-error "%s" "Number of fields must be between 2 and 100")
    (labels ((format-fields
              (fmt n)
              (with-temp-buffer
                (loop for i from 1 to n
                  do (insert
                      (let* ((result fmt)
                             (result (compat-replace-regexp-in-string
                                      result "%i" (int-to-string i))))
                        result)))
                (delete-char -2) ;; TBD
                (buffer-string))))
      (let ((start (point)))
        (loop for i from 2 to n do
          (unless (= i 2)
            (insert "\n"))
          (insert
           (let* ((result (nth 0 esml-gen-ftu-setters-template))
                  (result (compat-replace-regexp-in-string
                           result "%1" (format-fields (nth 1 esml-gen-ftu-setters-template) i)))
                  (result (compat-replace-regexp-in-string
                           result "%2" (format-fields (nth 2 esml-gen-ftu-setters-template) i)))
                  (result (compat-replace-regexp-in-string
                           result "%3" (format-fields (nth 3 esml-gen-ftu-setters-template) i)))
                  (result (compat-replace-regexp-in-string
                           result "%4" (format-fields (nth 4 esml-gen-ftu-setters-template) i)))
                  (result (compat-replace-regexp-in-string
                           result "%5" (format-fields (nth 5 esml-gen-ftu-setters-template) i)))
                  (result (compat-replace-regexp-in-string
                           result "%n" (int-to-string i))))
             result)))
        (indent-region start (point) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-gen)
