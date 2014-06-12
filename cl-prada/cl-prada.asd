;;;; cl-prada.asd

(asdf:defsystem #:cl-prada
  :serial t
  :description "Common Lisp interface to libPRADA"
  :author "Daniel Di Marco"
  :license "GPL"
  :depends-on (:com.gigamonkeys.parser)
  :components ((:file "package")
               ;; prada-path.lisp is generated automatically by CMakeLists.txt
               (:file "prada-path")
               (:file "utils")
               (:file "symbols")
               (:file "parser")
               (:file "state")
               (:file "rules")
               (:file "planner")
               (:file "learn")
               (:file "cl-prada")
               (:file "example")))

