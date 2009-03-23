;;; asdf system definition
(in-package #:asdf)

(defsystem :php
  :version "0.0.1"
  :licence "Public Domain / 0-clause MIT"
  :depends-on (:iterate-keywords
               :cl-utilities )
  :components
  ((:file "package")
   (:file "macros" :depends-on ("package"))
   (:file "characters" :depends-on ("macros"))
   (:file "hierarchical-packages" :depends-on ("package"))
   (:file "reader" :depends-on ("hierarchical-packages" "characters"))))
