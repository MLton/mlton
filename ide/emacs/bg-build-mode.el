;; Copyright (C) 2007-2008 Vesa Karvonen
;;
;; MLton is released under a HPND-style license.
;; See the file MLton-LICENSE for details.

(require 'compile)
(require 'bg-build-util)
(if (string-match "XEmacs" emacs-version)
    (require 'overlay))

;; This is a minor mode for ``handsfree'' background batch building.  See
;; http://mlton.org/EmacsBgBuildMode for further information.

;; XXX: Cleanup.
;; XXX: Combinators for making common project configurations:
;;      - E.g. grep for saved files from given file
;; XXX: Locate project file(s) automatically
;; XXX: Context menu to the mode line indicator
;; XXX: `mode-line-format' (XEmacs 21.4) support

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude

(defvar bg-build-load-time t)

(defun bg-build-set-custom-and-update (sym val)
  (custom-set-default sym val)
  (unless bg-build-load-time
    (bg-build-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup bg-build nil
  "A minor mode for ``handsfree'' background batch builds."
  :group 'compilation)

(defcustom bg-build-action-on-failure (function first-error)
  "Optional action to perform when build fails."
  :type `(choice
          (const :tag "None" ,(function (lambda () nil)))
          (function :tag "Action"))
  :group 'bg-build)

(defcustom bg-build-action-on-messages (function first-error)
  "Optional action to perform when build does not fail, but produces
messages (typically warnings)."
  :type `(choice
          (const :tag "None" ,(function (lambda () nil)))
          (function :tag "Action"))
  :group 'bg-build)

(defcustom bg-build-delay 1.0
  "Idle time in seconds to delay before automatically starting a build
after a save or nil if you wish to disable automatic builds."
  :type '(choice
          (const :tag "disable" nil)
          (number :tag "seconds"))
  :group 'bg-build)

(defcustom bg-build-key-bindings
  '()
  "Key bindings for the bg-build mode.  The key specifications must be in
a format accepted by the function `define-key'.  Hint: You might want to
type `M-x describe-function bg-build <TAB>' to see the available commands."
  :type '(repeat (cons :tag "Key Binding"
                       (string :tag "Key")
                       (function :tag "Command")))
  :set (function bg-build-set-custom-and-update)
  :group 'bg-build)

(defcustom bg-build-highlighting-overlay-priority 500
  "Priority of highlighting overlays."
  :type 'integer
  :group 'bg-build)

(defcustom bg-build-max-live-builds 1
  "Maximum number of live build processes to run concurrently or nil for
unlimited."
  :type '(choice
          (const :tag "Unlimited" nil)
          (number :tag "Number"))
  :group 'bg-build)

(defface bg-build-message-sexp-face
  '((((class color)) (:background "orange"))
    (t (:background "gray")))
  "Face for highlighting sexps that are referred to in messages."
  :group 'faces
  :group 'bg-build)

(defcustom bg-build-message-highlighting '(sexp)
  "How to highlight source locations corresponding to messages.  Unselect
all to disable highlighting."
  :type '(set (const :tag "Sexp" sexp))
  :group 'bg-build)

(defcustom bg-build-notify '(messages failure)
  "When to notify about completed builds."
  :type '(set (const :tag "Success"  success)
              (const :tag "Messages" messages)
              (const :tag "Failure"  failure))
  :group 'bg-build)

(defcustom bg-build-projects-auto-load nil
  "Automatic loading of `bg-build-projects-recent' at startup."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Enabled" t))
  :group 'bg-build)

(defcustom bg-build-projects-recent '()
  "Automatically updated list of BGB files currently or previously loaded.
This customization variable is not usually manipulated directly by the
user."
  :type '(repeat
          (file :tag "BGB file" :must-match t))
  :group 'bg-build)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Object

(defun* bg-build-prj (file &key name build? shell)
  "Creates a project object for bg-build."
  (list
   (cons 'name
         (cond ((functionp name)
                name)
               ((stringp name)
                (bg-build-const name))
               (t
                (bg-build-const
                 (file-name-nondirectory file)))))
   (cons 'build?
         (cond ((functionp build?)
                build?)
               (t
                (bg-build-const t))))
   (cons 'shell
         (cond ((functionp shell)
                shell)
               ((consp shell)
                (bg-build-const shell))
               ((stringp shell)
                (bg-build-const (split-string shell "[ \n\t]+")))
               (t
                (compat-error "Shell command required!"))))
   (cons 'attr
         (file-attributes file))))

(defun bg-build-call-prj (project fun &rest args)
  (let* ((file (car project))
         (directory (file-name-directory file)))
    (with-temp-buffer
      (setq default-directory directory)
      (apply (bg-build-assoc-cdr fun project) args))))

(defun bg-build-prj-name (project)
  (bg-build-call-prj project 'name))

(defun bg-build-prj-build? (project saved-files)
  (bg-build-call-prj project 'build? saved-files))

(defun bg-build-prj-shell (project)
  (bg-build-call-prj project 'shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Active Projects

(defvar bg-build-projects nil)

(defun bg-build-set-projects (projects &optional dont-save)
  (setq bg-build-projects projects)
  (when (and (not dont-save)
             bg-build-projects-auto-load)
    (customize-save-variable
     'bg-build-projects-recent
     (mapcar (function car) projects))))

(defvar bg-build-add-project-history nil)

(add-to-list 'auto-mode-alist '("\\.bgb$" . emacs-lisp-mode))

(defun bg-build-add-project (&optional file dont-save)
  "Adds a project file to bg-build minor mode.  This basically
reads and evaluates the first Emacs Lisp expression from specified file.
The expression should evaluate to a bg-build project object."
  (interactive)
  (cond
   ((not file)
    (bg-build-add-project
     (compat-read-file-name
      "Specify bg-build -file: " nil nil t nil 'bg-build-add-project-history)
     dont-save))
   ((not (and (file-readable-p file)
              (file-regular-p file)))
    (compat-error "Specified file is not a regular readable file"))
   (t
    (let* ((file (compat-abbreviate-file-name (file-truename file)))
           (directory (file-name-directory file))
           (data (with-temp-buffer
                   (buffer-disable-undo)
                   (insert-file-contents file)
                   (setq default-directory directory)
                   (goto-char (point-min))
                   (eval `(labels
                              ((bg-build
                                (&rest args)
                                (apply (function bg-build-prj) ,file args)))
                            ,(read (current-buffer)))))))
      (bg-build-set-projects
       (bg-build-replace-in-assoc bg-build-projects file data)
       dont-save))
    (bg-build-status-update))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running Builds

(defvar bg-build-finished-builds nil)

(defvar bg-build-live-builds nil)

(defun bg-build-interrupt-build (project)
  (let* ((file (car project))
         (proc (bg-build-assoc-cdr file bg-build-live-builds)))
    (cond
     ((and proc (compat-process-live-p proc))
      ;; Ok.  We interrupt the build.
      (interrupt-process proc))
     (proc
      ;; Hmm...  Shouldn't normally happen.  The sentinel is supposed
      ;; to remove the build from the live list, so probably something
      ;; unexpected occurred in the sentinel.
      (setq bg-build-live-builds
            (bg-build-remove-from-assoc
             bg-build-live-builds
             file))))
    (bg-build-check-build-queue)))

(defvar bg-build-messages nil)

(defun bg-build-parse-messages ()
  (let ((original-display-message
         (when (fboundp 'display-message)
           (symbol-function 'display-message))))
    (when (fboundp 'display-message)
      (fset 'display-message
            (function
             (lambda (label &rest args)
               (unless (eq label 'progress)
                 (apply original-display-message label args))))))
    (unwind-protect
        (compat-compilation-parse-errors)
      (when (fboundp 'display-message)
        (fset 'display-message original-display-message)))))

;; XXX: The following advice depends on the internals of the compilation mode.
(defadvice next-error (after bg-build-next-error activate)
  (with-current-buffer compilation-last-buffer
    (bg-build-highlight-messages)))

(defadvice compile-goto-error (after bg-build-compile-goto-error activate)
  (with-current-buffer compilation-last-buffer
    (bg-build-highlight-messages)))

(defvar bg-build-highlighting-overlays nil)

(defun bg-build-parse-message (message)
  (cond
   ((consp message)
    (let ((message (cdr message)))
      (cond
       ((markerp message)
        (let* ((buffer (marker-buffer message))
               (file (buffer-file-name buffer))
               (point (marker-position message))
               (pos (bg-build-point-to-pos point)))
          (list (cons file pos))))
       ((consp message)
        (list
         (cons (caar message)
               (cons (cadr message)
                     (1- (or (caddr message) 1)))))))))
   ((vectorp message)
    (list (cons (aref message 0)
                (cons (aref message 1) (aref message 2)))))))

(defun bg-build-delete-highlighting-overlays ()
  (mapc (function
         (lambda (maybe-overlay)
           (when (overlayp maybe-overlay)
             (delete-overlay maybe-overlay))))
        bg-build-highlighting-overlays)
  (setq bg-build-highlighting-overlays nil))

(defun bg-build-highlight-messages ()
  (when (and bg-build-messages
             bg-build-message-highlighting)
    (let ((file-to-buffer (bg-build-make-hash-table)))
      (mapc (function
             (lambda (buffer)
               (puthash (buffer-file-name buffer)
                        buffer
                        file-to-buffer)))
            (buffer-list))
      (setq bg-build-highlighting-overlays
            (mapcar (function
                     (lambda (info-or-overlay)
                       (if (overlayp info-or-overlay)
                           info-or-overlay
                         (let* ((info info-or-overlay)
                                (file (car info))
                                (pos (cdr info))
                                (buffer (gethash file file-to-buffer)))
                           (if (not buffer)
                               info-or-overlay
                             (with-current-buffer buffer
                               (let* ((begin
                                       (bg-build-pos-to-point pos))
                                      (beyond
                                       (save-excursion
                                         (goto-char begin)
                                         (condition-case ()
                                             (sml-user-forward-sexp) ;; XXX
                                           (error
                                            (condition-case ()
                                                (forward-sexp)
                                              (error
                                               (condition-case ()
                                                   (forward-word 1)
                                                 (error
                                                  ))))))
                                         (point)))
                                      (overlay
                                       (make-overlay begin beyond)))
                                 (overlay-put
                                  overlay 'priority
                                  bg-build-highlighting-overlay-priority)
                                 (overlay-put
                                  overlay 'face
                                  'bg-build-message-sexp-face)
                                 overlay)))))))
                    bg-build-highlighting-overlays)))))

(defun bg-build-process-sentinel (project)
  (lexical-let ((project project))
    (lambda (process event)
      (let ((event (upcase event))
            (file (car project))
            (buffer (process-buffer process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (compilation-mode)
            (compat-add-local-hook
             'kill-buffer-hook
             (bg-build-kill-buffer-hook project))
            (setq buffer-read-only nil)
            (let ((point (point)))
              (goto-char (point-max))
              (insert "\n" event)
              (goto-char point))
            (setq buffer-read-only t)
            (let ((previous (assoc file bg-build-finished-builds)))
              (when previous
                (kill-buffer (cdr previous))))
            (push (cons file buffer)
                  bg-build-finished-builds)
            (bg-build-parse-messages)
            (set (make-local-variable 'bg-build-messages)
                 (or (and (boundp 'compilation-locs)
                          (hash-table-p compilation-locs)
                          (let ((entries nil))
                            (maphash
                             (function
                              (lambda (key value)
                                (let* ((file (file-truename (caar value)))
                                       (lines (cddr value)))
                                  (mapc
                                   (function
                                    (lambda (line)
                                      (let ((locs (cdr line)))
                                        (mapc
                                         (function
                                          (lambda (loc)
                                            (push (vector
                                                   file
                                                   (or (cadr loc) 0)
                                                   (or (car loc) 0))
                                                  entries)))
                                         locs))))
                                   lines))))
                             compilation-locs)
                            entries))
                     (and (consp compilation-error-list)
                          compilation-error-list)))
            (set (make-local-variable 'bg-build-highlighting-overlays)
                 (apply
                  (function append)
                  (mapcar (function bg-build-parse-message)
                          bg-build-messages)))))
        (setq bg-build-live-builds
              (bg-build-remove-from-assoc bg-build-live-builds file))
        (bg-build-check-build-queue)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (bg-build-highlight-messages)))
        (cond
         ((string-match "EXITED ABNORMALLY WITH CODE \\([^\n]+\\)\n" event)
          (with-current-buffer buffer
            (condition-case ()
                (funcall bg-build-action-on-failure)
              (error
               )))
          (when (memq 'failure bg-build-notify)
            (message "FAILED, %d MESSAGE(S): %s"
                     (with-current-buffer buffer
                       (length bg-build-messages))
                     (bg-build-prj-name project))))
         ((and (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   bg-build-messages))
               (memq 'messages bg-build-notify)
               (string-match "FINISHED\n" event))
          (with-current-buffer buffer
            (funcall bg-build-action-on-messages))
          (message "%d MESSAGE(S): %s"
                   (with-current-buffer buffer
                     (length bg-build-messages))
                   (bg-build-prj-name project)))
         ((and (memq 'success bg-build-notify)
               (string-match "FINISHED\n" event))
          (message "SUCCEEDED: %s" (bg-build-prj-name project))))))))

(defun bg-build-kill-buffer-hook (project)
  (lexical-let ((project project))
    (lambda ()
      (let ((file (car project)))
        (setq bg-build-finished-builds
              (bg-build-remove-from-assoc bg-build-finished-builds file)))
      (bg-build-delete-highlighting-overlays)
      (bg-build-status-update))))

(defvar bg-build-counter 0)

(defun bg-build-start-build (project)
  (setq bg-build-counter (1+ bg-build-counter))
  (let* ((file (car project))
         (directory (file-name-directory file))
         (name (format "*%s (bg-build: %d)*"
                       (bg-build-prj-name project)
                       bg-build-counter))
         (shell (bg-build-prj-shell project)))
    (when (and name shell)
      (let* ((buffer (generate-new-buffer name))
             (process (with-current-buffer buffer
                        (buffer-disable-undo)
                        (compat-add-local-hook
                         'kill-buffer-hook
                         (bg-build-kill-buffer-hook project))
                        (insert "Compiling \"" file "\":\n\n")
                        (setq buffer-read-only t)
                        (setq default-directory directory)
                        (apply
                         (function start-process-shell-command)
                         name
                         buffer
                         shell))))
        (set-process-sentinel process (bg-build-process-sentinel project))
        (push (cons file process)
              bg-build-live-builds)))))

(defvar bg-build-build-queue nil)

(defun bg-build-check-build-queue ()
  (bg-build-status-update)
  (run-with-idle-timer
   0.01 nil
   (function
    (lambda ()
      (when (and bg-build-build-queue
                 (or (not bg-build-max-live-builds)
                     (< (length bg-build-live-builds)
                        bg-build-max-live-builds)))
        (bg-build-start-build (car (last bg-build-build-queue)))
        (setq bg-build-build-queue (butlast bg-build-build-queue))
        (bg-build-check-build-queue))))))

(defun bg-build-build-project (project)
  (setq bg-build-build-queue
        (bg-build-cons-once project bg-build-build-queue))
  (bg-build-interrupt-build project))

(defun bg-build-switch-to-messages (&optional other-window)
  "Switches to the latest finished build buffer with messages."
  (interactive "P")
  (let ((builds bg-build-finished-builds))
    (while (and builds
                (not (with-current-buffer (cdar builds)
                       bg-build-messages)))
      (pop builds))
    (if builds
        (let ((buffer (cdar builds)))
          (if other-window
              (switch-to-buffer-other-window buffer)
            (switch-to-buffer buffer)))
      (message "No messages"))))

(defun bg-build-switch-to-live (&optional other-window)
  "Switches to the latest live build buffer."
  (interactive "P")
  (if bg-build-live-builds
      (let ((buffer (process-buffer (cdar bg-build-live-builds))))
        (if other-window
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer)))
    (message "No live builds")))

(defun bg-build-switch-to-finished (&optional other-window)
  "Switches to the latest finished build buffer."
  (interactive "P")
  (if bg-build-finished-builds
      (let ((buffer (cdar bg-build-finished-builds)))
        (if other-window
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer)))
    (message "No finished builds")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic Build Triggering

(defvar bg-build-saved-files nil)

(defun bg-build-files-saved-timeout ()
  (mapc
   (function
    (lambda (project)
      (let ((file (car project))
            (data (cdr project)))
        (when (bg-build-attr-newer?
               (file-attributes file)
               (bg-build-assoc-cdr 'attr data))
          (bg-build-add-project file t)))))
   bg-build-projects)
  (let ((saved-files bg-build-saved-files))
    (setq bg-build-saved-files nil)
    (mapc
     (function
      (lambda (project)
        (when (bg-build-prj-build? project saved-files)
          (bg-build-build-project project))))
     bg-build-projects)))

(defvar bg-build-timer nil)

(defun bg-build-delete-timer ()
  (when bg-build-timer
    (compat-delete-timer bg-build-timer)
    (setq bg-build-timer nil)))

(defun bg-build-create-timer ()
  (bg-build-delete-timer)
  (when bg-build-delay
    (setq bg-build-timer
          (run-with-idle-timer
           bg-build-delay nil (function bg-build-files-saved-timeout)))))

(defun bg-build-after-save-hook ()
  (setq bg-build-saved-files
        (bg-build-cons-once
         (compat-abbreviate-file-name (file-truename (buffer-file-name)))
         bg-build-saved-files))
  (bg-build-create-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status Mode

(defconst bg-build-status-buffer-name "<:Bg-Build Status:>")

(defconst bg-build-status-mode-map
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result
               (read (car key-command))
               (cdr key-command))))
          `(("[(b)]"      . ,(function bury-buffer))
            ("[(q)]"      . ,(function bg-build-kill-current-buffer))
            ("[(a)]"      . ,(function bg-build-add-project))
            ("[(r)]"      . ,(function bg-build-status-rem-project))
            ("[(p)]"      . ,(function bg-build-status-visit-project-file))
            ("[(f)]"      . ,(function bg-build-status-visit-finished-build))
            ("[(l)]"      . ,(function bg-build-status-visit-live-build))
            ("[(return)]" . ,(function bg-build-status-start-build))))
    result))

(define-derived-mode bg-build-status-mode fundamental-mode "Bg-Build-Status"
  "Major mode for browsing bg-build related data."
  :group 'bg-build-status)

(defun bg-build-status ()
  "Show a buffer with bg-build mode related data."
  (interactive)
  (let ((buffer (get-buffer-create bg-build-status-buffer-name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq buffer-read-only t)
      (bg-build-status-mode))
    (switch-to-buffer buffer))
  (bg-build-status-update))

(defvar bg-build-status ""
  "Mode line status indicator for BGB mode")

(defun bg-build-status-update ()
  (let ((buffer (get-buffer bg-build-status-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((point (point)))
          (setq buffer-read-only nil)
          (goto-char 1)
          (delete-char (buffer-size))
          (insert "Status | Project
-------+------------------------------------------------------------------\n")
          (mapc (function
                 (lambda (project)
                   (let ((file (car project)))
                     (insert
                      (let ((n (length (member project bg-build-build-queue))))
                        (if (zerop n) "  " (format "%2d" n)))
                      (if (assoc file bg-build-live-builds) "L" " ")
                      (let ((buffer
                             (bg-build-assoc-cdr
                              file bg-build-finished-builds)))
                        (cond ((and buffer
                                    (with-current-buffer buffer
                                      bg-build-messages))
                               "FM")
                              (buffer
                               "F ")
                              (t
                               "  ")))
                      "  | "
                      (bg-build-prj-name project) " (" file ")"
                      "\n"))))
                bg-build-projects)
          (insert "\nTotal of " (number-to-string bg-build-counter)
                  " builds started.\n")
          (setq buffer-read-only t)
          (goto-char point)))))
  (setq bg-build-status
        (labels ((fmt (label n)
                      (cond ((= n 0) "")
                            ((= n 1) label)
                            (t (format "%s%d" label n)))))
          (let* ((queued (fmt "Q" (length bg-build-build-queue)))
                 (live (fmt "L" (length bg-build-live-builds)))
                 (messages
                  (let ((n (reduce
                            (function
                             (lambda (n build)
                               (with-current-buffer (cdr build)
                                 (+ n (length bg-build-messages)))))
                            bg-build-finished-builds
                            :initial-value 0)))
                    (if (and (= 0 n) bg-build-finished-builds)
                        "F"
                      (fmt "M" n))))
                 (str (concat "[" queued live messages "] ")))
            (if (string= str "[] ")
                ""
              str)))))

(defun bg-build-status-the-project ()
  (let ((idx (- (bg-build-current-line) 3)))
    (when (and (<= 0 idx)
               (< idx (length bg-build-projects)))
      (nth idx bg-build-projects))))

(defun bg-build-status-rem-project ()
  "Removes the project from bg-build."
  (interactive)
  (let ((project (bg-build-status-the-project)))
    (when project
      (bg-build-set-projects
       (bg-build-remove-from-assoc bg-build-projects (car project)))
      (bg-build-status-update))))

(defun bg-build-status-visit-project-file ()
  "Visits the project file of the project."
  (interactive)
  (let ((project (bg-build-status-the-project)))
    (when project
      (find-file (car project)))))

(defun bg-build-status-visit-finished-build ()
  "Visits the buffer of the finished build of the project."
  (interactive)
  (let ((project (bg-build-status-the-project)))
    (when project
      (let ((build (assoc (car project) bg-build-finished-builds)))
        (if build
            (switch-to-buffer (cdr build))
          (message "That project has no finished builds."))))))

(defun bg-build-status-visit-live-build ()
  "Visits the buffer of the live build of the project."
  (interactive)
  (let ((project (bg-build-status-the-project)))
    (when project
      (let ((build (assoc (car project) bg-build-live-builds)))
        (if build
            (switch-to-buffer (process-buffer (cdr build)))
          (message "That project has no live builds."))))))

(defun bg-build-status-start-build ()
  "Starts a new build of the project."
  (interactive)
  (let ((project (bg-build-status-the-project)))
    (when project
      (bg-build-build-project project))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun bg-build-mode-enabled-in-some-buffer ()
  (loop for buffer in (buffer-list) do
    (if (with-current-buffer buffer
          bg-build-mode)
        (return t))))

(defvar bg-build-mode-map (make-sparse-keymap)
  "Keymap for Background-Build mode.  This variable is updated by
`bg-build-build-mode-map'.")

(defun bg-build-build-mode-map ()
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result (read (car key-command)) (cdr key-command))))
          bg-build-key-bindings)
    (setq bg-build-mode-map result))
  (let ((cons (assoc 'bg-build-mode minor-mode-map-alist)))
    (when cons
      (setcdr cons bg-build-mode-map))))

(define-minor-mode bg-build-mode
  "Minor mode for performing builds on the background.

\\{bg-build-mode-map}
"
  :group 'bg-build
  :global t
  (remove-hook
   'after-save-hook (function bg-build-after-save-hook))
  (when (boundp 'mode-line-modes)
    (setq mode-line-modes
          (remove '(t bg-build-status) mode-line-modes)))
  (when (bg-build-mode-enabled-in-some-buffer)
    (add-hook
     'after-save-hook
     (function bg-build-after-save-hook))
    (when (boundp 'mode-line-modes)
      (add-to-list 'mode-line-modes '(t bg-build-status)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalization

(setq bg-build-load-time nil)

(defun bg-build-update ()
  "Update data based on customization variables."
  (bg-build-build-mode-map))

(bg-build-update)

(run-with-idle-timer
 1.0 nil
 (function
  (lambda ()
    (when bg-build-projects-auto-load
      (mapc (function
             (lambda (file)
               (when (and (file-readable-p file)
                          (file-regular-p file))
                 (bg-build-add-project file t))))
            bg-build-projects-recent)))))

(provide 'bg-build-mode)
