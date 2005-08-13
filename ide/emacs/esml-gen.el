;; Copyright (C) 2005 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(eval-when-compile
  (require 'cl))

;; Installation
;; ============
;;
;; Push the path to this file to `load-path' and use (require 'esml-gen).
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
(defconst esml-sml-symbolic-chars
  "-!%&$#+/:<=>?@~`^|*\\"
  "A string of all Standard ML symbolic characters as defined in section
2.4 of the Definition.")

(defconst esml-sml-alphanumeric-chars
  "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789'_"
  "A string of all Standard ML alphanumeric characters as defined in
section 2.4 of the Definition.")

(defun esml-extract-field-names (pattern-or-type)
  (let ((fields nil))
    (with-temp-buffer
      (insert pattern-or-type)
      (goto-char 0)
      (skip-chars-forward " \t\n{},")
      (while (not (eobp))
        (let ((start (point)))
          (if (find (char-after) esml-sml-symbolic-chars)
              (skip-chars-forward esml-sml-symbol-chars)
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
  '("fun set f =\nlet\nfun t2r (%1$s) = {%2$s}\nfun r2t {%3$s} = (%4$s)
in\nRecords.wrapSet (Tuples.set%5$i, t2r, t2r, r2t) f\nend\n"
    "v%1$i, "
    "%2$s = v%1$i, "
    "%2$s = v%1$i, "
    "v%1$i, ")
  "Template for `esml-gen-fru-setter'. Indentation is automatic. The last
two characters of a pattern are deleted at the end."
  :type '(list :tag "Template"
               (string :tag "Code (`%1$s' = 1., `%2$s' = 2., ..., `%5$i' = n)")
               (string :tag "1. pattern (`%1$i' = index, `%2$s' = name)")
               (string :tag "2. pattern (`%1$i' = index, `%2$s' = name)")
               (string :tag "3. pattern (`%1$i' = index, `%2$s' = name)")
               (string :tag "4. pattern (`%1$i' = index, `%2$s' = name)"))
  :group 'esml-gen)

(defun esml-gen-fru-setter (pattern-or-type)
  "Generates a functional record update function. The parameter must be in
the format `[{]id[: ty][,] ...[,] id[}]' where `[]' marks optional parts."
  (interactive "sSimple record pattern or type: ")
  (let* ((fields (esml-extract-field-names pattern-or-type))
         (n (length fields)))
    (if (< n 2)
        (error 'invalid-argument "Record must have at least two fields.")
      (let ((fields (sort fields 'string-lessp))
            (start (point)))
        (labels ((format-fields (fmt) (with-temp-buffer
                                        (loop
                                          for f in fields
                                          for i from 1 to n
                                          do (insert (format fmt i f)))
                                        (delete-char -2) ;; TBD
                                        (buffer-string))))
          (insert
           (format (nth 0 esml-gen-fru-setter-template)
                   (format-fields (nth 1 esml-gen-fru-setter-template))
                   (format-fields (nth 2 esml-gen-fru-setter-template))
                   (format-fields (nth 3 esml-gen-fru-setter-template))
                   (format-fields (nth 4 esml-gen-fru-setter-template))
                   n))
          (indent-region start (point) nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Tuple Update (see http://mlton.org/FunctionalRecordUpdate)

(defcustom esml-gen-ftu-setters-template
  '("fun set%6$i f v (%1$s) =\nlet\ndatatype (%2$s) t =\n%3$s
fun g h v =\n(%4$s)\nin\nf (%5$s) v\nend\n"
    "v%1$i, "
    "'v%1$i, "
    " V%1$i of 'v%1$i |"
    "case h v of V%1$i v%1$i => v%1$i | _ => v%1$i,\n"
    "g V%1$i, ")
  "Template for `esml-gen-ftu-setters'. Indentation is automatic. The last
two characters of a pattern are deleted at the end."
  :type '(list :tag "Format"
               (string :tag "Code (`%1$s' = 1., `%2$s' = 2., ..., `%6$i' = n)")
               (string :tag "1. pattern (`%1$i' = index)")
               (string :tag "2. pattern (`%1$i' = index)")
               (string :tag "3. pattern (`%1$i' = index)")
               (string :tag "4. pattern (`%1$i' = index)")
               (string :tag "5. pattern (`%1$i' = index)"))
  :group 'esml-gen)

(defun esml-gen-ftu-setters (n)
  "Generates functional tuple update, or `set<N>', functions."
  (interactive "nMaximum number of fields [2-100]: ")
  (if (not (and (<= 2 n)
                (<= n 100)))
      (error 'invalid-argument "Number of fields must be between 2 and 100.")
    (labels ((format-fields (fmt n) (with-temp-buffer
                                      (loop for i from 1 to n
                                        do (insert (format fmt i)))
                                      (delete-char -2) ;; TBD
                                      (buffer-string))))
      (let ((start (point)))
        (loop for i from 2 to n do
          (unless (= i 2)
            (insert "\n"))
          (insert
           (format (nth 0 esml-gen-ftu-setters-template)
                   (format-fields (nth 1 esml-gen-ftu-setters-template) i)
                   (format-fields (nth 2 esml-gen-ftu-setters-template) i)
                   (format-fields (nth 3 esml-gen-ftu-setters-template) i)
                   (format-fields (nth 4 esml-gen-ftu-setters-template) i)
                   (format-fields (nth 5 esml-gen-ftu-setters-template) i)
                   i)))
        (indent-region start (point) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esml-gen)
