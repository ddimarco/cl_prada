(in-package :cl-prada)

(defclass prada-learn-state ()
  ((world :initarg :world :initform nil)
   (action :initarg :action :initform nil)
   (weight :initarg :weight :initform 1.0)
   (world-after :initarg :world-after :initform nil)
   (comment :initarg :comment :initform nil)))

(defun world-state-difference (learn-state)
  (flet ((prefix-predicate (pred symbol)
           (append (list (intern (format nil "~a~a" symbol (car pred))))
                   (cdr pred))))
   (let ((ws1 (slot-value learn-state 'world))
         (ws2 (slot-value learn-state 'world-after)))
     (append (mapcar (lambda (rel) (prefix-predicate rel #\-))
                     (set-difference ws1 ws2 :test #'equal))
             (mapcar (lambda (rel) (prefix-predicate rel #\+))
                     (set-difference ws2 ws1 :test #'equal))))))

(defun read-prada-learn-data (file)
  (with-open-file (infile file :direction :input)
    ;; comment with timestamp
    (loop with state = nil
       with action = nil
       with result = nil
       for line = (read-line infile nil nil)
         until (null line)
         when (char/= (elt line 0) #\#)
         do
         (multiple-value-bind (ok parsed)
             (world-state-parser:world-state-parser line)
           ;; HACK: should be fixed in the parser instead
           ;; (setf parsed (remove "()" parsed :test #'equal))
           (cond
             ((null ok)
              (error "error when parsing line ~a" line))
             ((null state)
              (setf state parsed))
             ((null action)
              ;; action should be a single one
              (setf action (car parsed))))
           (when (and action state)
             (push (make-instance 'prada-learn-state :action action :world state)
                   result)
             (setf state nil
                   action nil)))
       finally (progn
                 (when state
                   (push (make-instance 'prada-learn-state :world state) result))
                 (return (reverse result))))))

(defun write-learn-data-as-lisp (data)
  (labels ((internize-symbols (lst)
             (etypecase lst
               (list
                (loop for s in lst
                   collect (internize-symbols s)))
               (symbol
                (intern (symbol-name lst))))))
   (with-slots (world world-after action)
       data
     `(make-instance 'experience :world ',(internize-symbols world)
                     :action ',(internize-symbols action)
                     :world-after ',(internize-symbols world-after)))))
;; (with-open-file (out "/tmp/bla" :direction :output :if-exists :supersede)
;;             (format out "(defparameter *exps* (list~%")
;;             (dolist (e *exps*)
;;               (print (write-learn-data-as-lisp e) out))
;;             (format out "))~%"))

;;(defvar +learner-path+ "/home/marcodl/code/cl-prada/libPRADA/bin/relational_learn")
(defun learner-path ()
  (concatenate 'string *prada-path* "/relational_learn"))

(defun write-learn-file (out-file learn-data)
  (with-open-file (data-file out-file :direction :output :if-exists :supersede)
    (with-open-file (orig-data-file "/tmp/orig.dat" :direction :output :if-exists :supersede)
      (loop for exp in learn-data
         for i from 0
         for ws = (slot-value exp 'world)
         for action = (slot-value exp 'action)
         for world-after = (slot-value exp 'world-after)
         for common-symbol->num = (slot-value (make-prada-world-state (append ws world-after))
                                              'symbol->num) do
           (format data-file "# t=~a; ~a~%" i (or (slot-value exp 'comment) ""))
           (format orig-data-file "# t=~a~%" i)
           (write-prada-world-state
            (make-instance 'prada-state :state ws :symbol->num common-symbol->num)
            data-file nil)
           (format data-file "~%")
           (when action
             (write-prada-world-state
              (make-instance 'prada-state :state (list action)
                             :symbol->num common-symbol->num)
              data-file nil)
             (format data-file "~%")
             (format orig-data-file "~a~%" action))
           (format orig-data-file "~a~%" (delispify-expr (world-state-difference exp)))
           (when world-after
             (write-prada-world-state
              (make-instance 'prada-state :state world-after
                             :symbol->num common-symbol->num)
              data-file nil)
             (format data-file "~%"))))))

(defun write-prada-files-learning (learn-data symbol-defs)
  (with-open-file (symbols-file "/tmp/symbols.dat" :direction :output :if-exists :supersede)
    (write-prada-symbol-defs symbol-defs symbols-file))
  (write-learn-file "/tmp/data.dat" learn-data)
  ;; (with-open-file (data-file "/tmp/data.dat" :direction :output :if-exists :supersede)
  ;;   (with-open-file (orig-data-file "/tmp/orig.dat" :direction :output :if-exists :supersede)
  ;;     (loop for exp in learn-data
  ;;        for i from 0
  ;;        for ws = (slot-value exp 'world)
  ;;        for action = (slot-value exp 'action)
  ;;        for world-after = (slot-value exp 'world-after)
  ;;        for common-symbol->num = (slot-value (make-prada-world-state (append ws world-after))
  ;;                                             'symbol->num) do
  ;;          (format data-file "# t=~a; ~a~%" i (or (slot-value exp 'comment) ""))
  ;;          (format orig-data-file "# t=~a~%" i)
  ;;          (write-prada-world-state
  ;;           (make-instance 'prada-state :state ws :symbol->num common-symbol->num)
  ;;           data-file nil)
  ;;          (format data-file "~%")
  ;;          (when action
  ;;            (write-prada-world-state
  ;;             (make-instance 'prada-state :state (list action)
  ;;                            :symbol->num common-symbol->num)
  ;;             data-file nil)
  ;;            (format data-file "~%")
  ;;            (format orig-data-file "~a~%" action))
  ;;          (format orig-data-file "~a~%" (delispify-expr (world-state-difference exp)))
  ;;          (when world-after
  ;;            (write-prada-world-state
  ;;             (make-instance 'prada-state :state world-after
  ;;                            :symbol->num common-symbol->num)
  ;;             data-file nil)
  ;;            (format data-file "~%")))))
  )

(defun parsed-rules->rules (parsed)
  (loop for p in parsed
       for (action context outcomes) = p collect
       (make-instance 'rule :action action :context context :outcomes outcomes)))

(defun run-learner (experiences symbol-defs &key (alpha-pen nil) (p-min nil)
                                              (output-file "/tmp/learned_rules.dat"))
  (write-prada-files-learning experiences symbol-defs)
  (if (probe-file output-file)
      (delete-file output-file))
  (let ((parameter-string (format nil "~a ~a -o ~a"
                                  (if alpha-pen
                                      (format nil "--alpha-pen ~a" alpha-pen)
                                      "")
                                  (if p-min
                                      (format nil "--p-min ~a" p-min)
                                      "")
                                  output-file)))
    (let ((output-string
           (with-output-to-string (asdf::*verbose-out*)
             (asdf:run-shell-command "cd /tmp; ~a ~a"
                                     (learner-path)
                                     parameter-string))))
      (with-open-file (log "/tmp/prada-output.log" :direction :output :if-exists :supersede)
        (format log "~a~%" output-string))
      (unless (probe-file output-file)
        (format t "~a~%" output-string)
        (error "no result file found. check output above."))
      (multiple-value-bind (parsed result)
          (world-state-parser:rules-parser (slurp-text-file output-file))
        (unless parsed
          (format t "~a~%" output-string)
          (error "could not parse result. check output above."))
        (parsed-rules->rules result)))))

(defun summarize-experiences (experiences)
  (let ((actions-experiences (make-hash-table)))
    (loop for exp in experiences
         for action = (slot-value exp 'action)
         do
         (push exp (gethash (car action) actions-experiences)))
    (dolist (action (alexandria:hash-table-keys actions-experiences))
      (format t "action: ~a; experiences: ~a~%" action (length (gethash action actions-experiences))))))

(defun show-experiences-of-type (action experiences)
  (loop for exp in experiences
     for exp-action = (slot-value exp 'action)
     when (eq action (car exp-action))
     do
       (format t "~a~%~a~%~%"
               exp-action
               (world-state-difference exp))))
