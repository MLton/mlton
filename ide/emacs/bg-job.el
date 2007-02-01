;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background Processor

(defun bg-job-start (done? step finalize &rest args)
  "Starts a background job.  The job is considered active as longs as

  (apply done? args)

returns nil.  While the job is active,

  (apply step args)

will be called periodically to perform a (supposedly small) computation
step.  The return value, which must be a list, will be used as the next
args.  So, a step function often looks like this:

  (function
   (lambda (args)
     ;; do something
     (list args)))

After the job becomes inactive,

  (apply finalize args)

will be called once and the job will be discarded.

A job may call `bg-job-start' to start new jobs and multiple background
jobs may be active simultaneously."
  (push (cons args (cons done? (cons step finalize))) bg-job-queue)
  (bg-job-reschedule))

(defun bg-job-done? (job)
  (apply (cadr job) (car job)))

(defun bg-job-step (job)
  (setcar job (apply (caddr job) (car job))))

(defun bg-job-finalize (job)
  (apply (cdddr job) (car job)))

(defvar bg-job-queue nil)
(defvar bg-job-timer nil)

(defconst bg-job-period 0.03)
(defconst bg-job-cpu-ratio 0.3)

(defun bg-job-reschedule ()
  (unless bg-job-timer
    (setq bg-job-timer
          (run-with-timer
           (/ bg-job-period bg-job-cpu-ratio)
           nil
           (function bg-job-quantum)))))

(defun bg-job-quantum ()
  (let ((start-time (bg-job-time-to-double (current-time))))
    (while (and bg-job-queue
                (< (- (bg-job-time-to-double (current-time)) start-time)
                   bg-job-period))
      (let ((job (pop bg-job-queue)))
        (if (bg-job-done? job)
            (bg-job-finalize job)
          (bg-job-step job)
          (setq bg-job-queue (nconc bg-job-queue (list job)))))))
  (setq bg-job-timer nil)
  (when bg-job-queue
    (bg-job-reschedule)))

(defun bg-job-time-to-double (time)
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (caddr time) 1000000.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bg-job)
