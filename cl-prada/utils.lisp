(in-package :cl-prada)

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun delispify-expr (expr &key (commas nil))
  (etypecase expr
    (list
     (let ((head (car expr))
           (rest (cdr expr)))
       (case head
         (and
          (mapcar #'delispify-expr rest))
         (not
          (assert (null (cdr rest)))
          (format nil "-~a" (delispify-expr (car rest))))
         (=
          (assert (= (length rest) 2))
          (format nil "~a=~a" (delispify-expr (car rest)) (delispify-expr (cadr rest))))
         (t
          ;; predicate, rest should be variables or constants
          (format nil "~(~a~)~a" head
                  (if rest
                      (format
                       nil
                       (if commas
                           "(~{~a~^,~})"
                           "(~{~a~^ ~})")
                       (loop for e in rest collect (delispify-expr e :commas commas)))
                      "()"))))))
    (symbol
     (cond
       ((char= (elt (symbol-name expr) 0) #\?)
        ;; variable
        (subseq (symbol-name expr) 1))
       (t expr)))
    (number
     expr)))

(defun slurp-text-file (file)
  (with-open-file (in file :direction :input)
    (let ((seq (make-string (file-length in))))
      (read-sequence seq in)
      seq)))

(defun get-all-identifiers (tree)
  (typecase tree
    (list
     (loop for e in (cdr tree) append
          (get-all-identifiers e)))
    (symbol
     (list tree))
    (t nil)))
