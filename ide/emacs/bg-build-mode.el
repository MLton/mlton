;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

(require 'cl)
(require 'compile)
(require 'compat)

;; This is a minor mode for ``handsfree'' background batch building.  See
;; http://mlton.org/EmacsBgBuildMode for further information.

;; NOTE: This mode is not yet quite complete!  Expect several crucial
;; usability improvements in the near future.
;;
;; XXX: Mode for status display, navigation, and removing of project cfgs
;; XXX: Commands: goto-last-build-buffer, start-build
;; XXX: Better compilation-mode:
;;      - Give count of warnings and errors
;;      - Highlighting in XEmacs
;; XXX: Reload project file automatically
;; XXX: Combinators for making common project configurations:
;;      - E.g. grep for saved files from given file
;; XXX: Highlight (lines with) errors and warnings
;; XXX: Locate project file(s) automatically

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
  "A minor mode for ``handsfree'' background batch builds.

Functionality:
- Each time a file is saved, and after a user configurable delay period
  has been exhausted, a build is started silently in the background.
- When the build is finished, a status indicator is displayed
  non-intrusively.
- At any time, you can switch to a bg-build buffer where all the messages
  from the build are shown.
- After the build has finished you can jump to locations of warnings and
  errors from the bg-build buffer or by using the `first-error' and
  `next-error' commands.
- When starting a build of a certain project, a possible previous live
  build of the same project is killed first.
- A project configuration file specifies the commands required to build a
  project."
  :group 'compilation)

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

(defcustom bg-build-max-live-builds 1
  "Maximum number of live build processes to run concurrently or nil for
unlimited."
  :type '(choice
          (const :tag "Unlimited" nil)
          (number :tag "Number"))
  :group 'bg-build)

(defcustom bg-build-notify 'on-failure
  "When to notify about completed builds."
  :type '(choice
          (const :tag "Always" always)
          (const :tag "Never" never)
          (const :tag "On failure" on-failure))
  :group 'bg-build)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun bg-build-cons-once (entry list)
  (cons entry (remove* entry list :test (function equal))))

(defun bg-build-flatmap (fn list)
  (apply (function append) (mapcar fn list)))

(defun bg-build-remove-from-assoc (alist key)
  (remove*
   nil alist
   :test (function
          (lambda (_ key-value)
            (equal key (car key-value))))))

(defun bg-build-replace-in-assoc (alist key value)
  (cons (cons key value)
        (bg-build-remove-from-assoc alist key)))

(defun bg-build-assoc-cdr (key alist)
  "Same as (cdr (assoc key alist)) except that doesn't attempt to call cdr
on nil."
  (let ((key-value (assoc key (cdr alist))))
    (when key-value
      (cdr key-value))))

(defun bg-build-const (value)
  "Returns a function that returns the given value."
  (lexical-let ((value value))
    (lambda (&rest _)
      value)))

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
                (compat-error "Shell command required!"))))))

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

(defun bg-build-add-project (file)
  "Adds a project file to bg-build minor mode.  This basically
reads and evaluates the first Emacs Lisp expression from specified file.
The expression should evaluate to a bg-build project object."
  (interactive "fSpecify bg-build -file: ")
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
    (setq bg-build-projects
          (bg-build-replace-in-assoc bg-build-projects file data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running Builds

(defvar bg-build-finished-builds nil)

(defvar bg-build-live-builds nil)

(defun bg-build-interrupt-build (project)
  (let* ((file (car project))
         (live (assoc file bg-build-live-builds)))
    (if live
        (interrupt-process (cdr live)))
    (bg-build-check-build-queue)))

(defun bg-build-process-sentinel (project)
  (lexical-let ((project project))
    (lambda (process event)
      (let ((event (upcase event))
            (file (car project))
            (buffer (process-buffer process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (compilation-mode)
            (setq buffer-read-only nil)
            (let ((point (point))
                  (point-max (point-max)))
              (goto-char point-max)
              (insert "\n" event)
              (if (= point point-max)
                  (goto-line 3)
                  (goto-char point)))
            (setq buffer-read-only t)
            (let ((previous (assoc file bg-build-finished-builds)))
              (when previous
                (kill-buffer (cdr previous))))
            (push (cons file buffer)
                  bg-build-finished-builds)))
        (setq bg-build-live-builds
              (bg-build-remove-from-assoc bg-build-live-builds file))
        (cond
         ((and (memq bg-build-notify '(always))
               (string-match "FINISHED\n" event))
          (message "SUCCEEDED: %s" (bg-build-prj-name project)))
         ((and (memq bg-build-notify '(always on-failure))
               (string-match "EXITED ABNORMALLY WITH CODE \\([^\n]+\\)\n" event))
          (message "FAILED: %s" (bg-build-prj-name project)))))
      (bg-build-check-build-queue))))

(defun bg-build-kill-buffer-hook (project)
  (lexical-let ((project project))
    (lambda ()
      (let ((file (car project)))
        (setq bg-build-finished-builds
              (bg-build-remove-from-assoc bg-build-finished-builds file))))))

(defvar bg-build-counter 0)

(defun bg-build-start-build (project)
  (setq bg-build-counter (1+ bg-build-counter))
  (let* ((file (car project))
         (directory (file-name-directory file))
         (name (format "*%s (bg-build: %d)*" (bg-build-prj-name project) bg-build-counter))
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
  (while (and bg-build-build-queue
              (or (not bg-build-max-live-builds)
                  (< (length bg-build-live-builds)
                     bg-build-max-live-builds)))
    (bg-build-start-build (car (last bg-build-build-queue)))
    (setq bg-build-build-queue (butlast bg-build-build-queue))))

(defun bg-build-build-project (project)
  (setq bg-build-build-queue
        (bg-build-cons-once project bg-build-build-queue))
  (bg-build-interrupt-build project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic Build Triggering

(defvar bg-build-saved-files nil)

(defun bg-build-files-saved-timeout ()
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
  :lighter " BGB"
  :group 'bg-build
  :global t
  (remove-hook
   'after-save-hook (function bg-build-after-save-hook))
  (when (bg-build-mode-enabled-in-some-buffer)
    (add-hook
     'after-save-hook
     (function bg-build-after-save-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalization

(setq bg-build-load-time nil)

(defun bg-build-update ()
  "Update data based on customization variables."
  (bg-build-build-mode-map))

(bg-build-update)

(provide 'bg-build-mode)
