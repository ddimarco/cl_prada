(in-package :cl-prada)

(defclass prada-symbol ()
  ((name :initarg :name :initform (error "you need to specify a name") :accessor name)
   (arity :initarg :arity :accessor arity)
   (type :initarg :type)
   (value-type :initarg :value-type)))

(defclass prada-conjunction (prada-symbol)
  ((base-expr :initarg :base-expr)
   (all-quantified :initarg :all-quantified :initform nil)))

(defclass prada-transclosure (prada-symbol)
  ((base-literal :initarg :base-literal)))

(defclass prada-count (prada-symbol)
  ((base-expr :initarg :base-expr)
   (variable :initarg :variable)))

(defclass prada-sum (prada-symbol)
  ((base-expr :initarg :base-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric write-prada-symbol (symbol))

(defmethod write-prada-symbol ((symbol prada-conjunction))
  (write-basic-prada-symbol symbol)
  (with-slots (base-expr all-quantified) symbol
    (format t " <-- ")
    (when all-quantified
      (unless (null (cdr all-quantified))
        (error "only can have 1 all-quantified variable"))
      (format t "All ~a " (delispify-expr (car all-quantified))))
    (format t "~{~a ~}~%" (ensure-list (delispify-expr base-expr)))))

(defmethod write-prada-symbol ((symbol prada-transclosure))
  ;; needs a +/- with a space
  (write-basic-prada-symbol symbol)
  (with-slots (base-literal) symbol
    (format t " <-- ")
    (let* ((negated (and (listp base-literal)
                         (eq (car base-literal) 'not)))
           (base-literal (if negated
                             (cadr base-literal)
                             base-literal)))
      (format t "~a ~(~a~)~%"
              (if negated "-" "+")
              (delispify-expr base-literal)))))

(defmethod write-prada-symbol ((symbol prada-count))
  (write-basic-prada-symbol symbol)
  (with-slots (base-expr variable) symbol
    (format t " <-- ")
    (format t "Num ~a ~a~%"
            (delispify-expr variable)
            (delispify-expr base-expr))))

(defmethod write-prada-symbol ((symbol prada-sum))
  (write-basic-prada-symbol symbol)
  (with-slots (base-expr) symbol
    (format t " <-- ")
    ;; must only be the symbol
    (let ((expr (delispify-expr base-expr)))
      (format t "Sum ~a~%"
              (subseq expr 0 (position #\( expr))))))

(defmethod write-prada-symbol ((symbol prada-symbol))
  (write-basic-prada-symbol symbol)
  (format t "~%"))

(defun write-basic-prada-symbol (symbol &optional (outstream t))
  (with-slots (name arity type value-type) symbol
      (format outstream "~(~a ~a ~a ~a~)" name arity type value-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-symbol-def (def)
  (let ((type (car def))
        (rest (cdr def)))
    (ecase type
      (action
       (destructuring-bind (name arity) rest
         (make-instance 'prada-symbol :name name :arity arity :type :action
                        :value-type :binary)))
      (primitive
       (destructuring-bind (name arity &key (value-type :binary)) rest
           (make-instance 'prada-symbol :name name :arity arity :type :primitive
                          :value-type value-type)))
      (conjunction
       (destructuring-bind (name arity &key <- all) rest
         (make-instance 'prada-conjunction :name name :arity arity :type :conjunction
                        :value-type :binary
                        :base-expr <-
                        :all-quantified (if (not (listp all))
                                            (list all)
                                            all))))
      (transclosure
       (destructuring-bind (name arity &key <-) rest
         (make-instance 'prada-transclosure :name name :arity arity :type :transclosure
                        :value-type :binary
                        :base-literal <-)))
      (count
       (destructuring-bind (name arity &key <- num) rest
         (make-instance 'prada-count :name name :arity arity :type :count
                        :value-type :integers
                        :base-expr <-
                        :variable num)))
      (sum
       (destructuring-bind (name arity &key <-) rest
         (make-instance 'prada-sum :name name :arity arity :type :sum
                        :value-type :integers
                        :base-expr <-))))))
