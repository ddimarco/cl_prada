(in-package :cl-prada)

(defvar *domains* (make-hash-table))

(defun get-domain (name)
  (gethash name *domains*))

(defclass prada-domain ()
  ((name :initarg :name :initform (error "you need to specify a name") :accessor name)
   (symbols :initarg :symbols)
   (rules :initarg :rules)
   (world-state :initarg :world-state)))

(defmacro with-output-to-stream-or-stdout (stream &body body)
  (let ((stream-name (gensym)))
    `(let ((,stream-name ,stream))
       (write-string
        (with-output-to-string (*standard-output*)
          ,@body)
        (if ,stream-name
            ,stream-name
            *standard-output*)))))

(defun write-prada-symbol-defs (symbol-defs stream)
  (with-output-to-stream-or-stdout stream
    (dolist (s symbol-defs)
      (write-prada-symbol s))))

(defun write-prada-domain (domain &key (symbols-stream nil)
                                    (rules-stream nil)
                                    (state-stream nil))
  (with-slots (name symbols rules world-state)
      domain
    (write-prada-symbol-defs symbols symbols-stream)
    (with-output-to-stream-or-stdout rules-stream
      (dolist (r rules)
        (write-prada-rule r)))
    (with-output-to-stream-or-stdout state-stream
      (write-prada-world-state world-state))))

(defmacro def-prada-domain (name symbol-defs rule-defs state)
  (setf (gethash name *domains*)
        (make-instance 'prada-domain :name name
                       :symbols
                       (loop for def in symbol-defs
                          collect (make-symbol-def def))
                       :rules
                       (loop for def in rule-defs
                          collect (make-rule-def def))
                       :world-state
                       (init-symbols->id-map
                        (make-instance 'prada-state :state state)))))
