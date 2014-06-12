(in-package :cl-prada)

(defclass prada-state ()
  ((symbol->num :initform (make-hash-table) :initarg :symbol->num)
   (state :initarg :state)))

(defun init-symbols->id-map (prada-state)
  (with-slots (symbol->num state) prada-state
    (let ((max-id 20))
      (loop for statement in state
           for identifiers = (get-all-identifiers statement)
           do
         ;; create numids for symbols
           (dolist (id identifiers)
             (when (null (gethash id symbol->num))
               (setf (gethash id symbol->num)
                     (incf max-id)))))))
  prada-state)

(defun convert-symbols->num-ids (symbol->num literals)
  (loop for statement in literals
         for identifiers = (get-all-identifiers statement)
         collect
       (loop for id in identifiers
          for num = (gethash id symbol->num)
          with new-stmt = statement
          do
            ;; (when (null num)
            ;;   (error "cannot convert symbol ~a to numerical id!" id))
            (setf new-stmt (subst num id new-stmt))
          finally (return new-stmt))))

(defun pradaify-world-state (world-state)
  (with-slots (symbol->num state) world-state
    (convert-symbols->num-ids symbol->num state)))

(defun write-prada-world-state (world-state &optional (stream t) (commas t))
  (dolist (stmt (pradaify-world-state world-state))
      (format stream "~a " (delispify-expr stmt :commas commas))))

(defun read-prada-world-state (file)
  ;; no negative binary symbols
  (let ((slurped (slurp-text-file file)))
    (multiple-value-bind (success lispified-state)
        (world-state-parser:world-state-parser slurped)
      (unless success
        (error "could not parse world state"))
      lispified-state)))

(defun make-prada-world-state (state-lst)
  (init-symbols->id-map
   (make-instance 'prada-state :state state-lst)))
