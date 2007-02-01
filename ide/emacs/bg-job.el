;; Copyright (C) 2007 Vesa Karvonen
;;
;; MLton is released under a BSD-style license.
;; See the file MLton-LICENSE for details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background Processor

(defun bg-job-start (done? step finalize &rest args)
  "Starts a background job."
  (push (cons args (cons done? (cons step finalize))) bg-jobs)
  (unless (cdr bg-jobs)
    (bg-job-reschedule)))

(defun bg-job-done? (job)
  (apply (cadr job) (car job)))

(defun bg-job-step (job)
  (setcar job (apply (caddr job) (car job))))

(defun bg-job-finalize (job)
  (apply (cdddr job) (car job)))

(defvar bg-jobs nil)

(defconst bg-job-period 0.03)
(defconst bg-job-cpu-ratio 0.3)

(defun bg-job-reschedule ()
  (when bg-jobs
    (run-with-timer
     (/ bg-job-period bg-job-cpu-ratio)
     nil
     (function bg-job-quantum))))

(defun bg-job-quantum ()
  (let ((start-time (bg-job-time-to-double (current-time))))
    (while (and bg-jobs
                (< (- (bg-job-time-to-double (current-time))
                      start-time)
                   bg-job-period))
      (let ((job (pop bg-jobs)))
        (if (bg-job-done? job)
            (bg-job-finalize job)
          (bg-job-step job)
          (setq bg-jobs
                (nconc bg-jobs (list job)))))))
  (bg-job-reschedule))

(defun bg-job-time-to-double (time)
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (caddr time) 1000000.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bg-job)
