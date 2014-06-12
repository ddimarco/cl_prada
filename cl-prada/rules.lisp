(in-package :cl-prada)

(defclass rule ()
  ((action :initarg :action)   ;; instantiated predicate
   (context :initarg :context) ;; ~ precondition
   ;; lists of (probability primitive-literals)
   (outcomes :initarg :outcomes)))

(defmethod print-object ((object Rule) stream)
  (format stream "[ACTION:~%  ~(~a~)~%CONTEXT:~%  ~(~a~)~%OUTCOMES:~%" (slot-value object 'action)
          (slot-value object 'context))
  (loop for o in (slot-value object 'outcomes)
       for i from 0 do
    (format stream "  ~a ~(~a~)~@[]~]~%" (car o) (cadr o)
            (= i (1- (length (slot-value object 'outcomes)))))))

(defun make-rule-def (def)
  (assert (eq (car def) 'action))
  (destructuring-bind (head-expr context-expr outcomes) (cdr def)
    (make-instance 'rule
                   :action head-expr
                   :context context-expr
                   :outcomes outcomes)))

(defun write-prada-rule (rule)
  (with-slots (action context outcomes) rule
    (format t "ACTION:~%  ~a~%CONTEXT:~%  ~{~a~^ ~}~%OUTCOMES:~%"
            (delispify-expr action)
            (ensure-list
             (if context-expr
                 (delispify-expr context)
                 "--")))
    ;; check that probabilities sum to 1
    (unless (< (abs
                (- (loop for o in outcomes
                    for (prob expr) = o
                    sum prob)
                   1.0))
               0.001)
      (error (format nil "rule outcomes do not sum to 1.0: ~a" outcomes)))
    (let ((noise-prob (car (find :noise outcomes :key #'cadr))))
      (assert noise-prob)
      (loop for o in outcomes
         for (prob expr) = o
         unless (eq expr :noise)
         do
           (check-type prob number)
           (format t "  ~a ~{~a~^ ~}~%" prob
                   (ensure-list
                    (if (eq expr :no-change)
                        "<no-change>"
                        (delispify-expr expr)))))
      (format t "  ~a <noise>~%" noise-prob))))
