(in-package :php)


(defpackage :package1.package2.package1
  (:use :cl)
  (:export #:foo))
(in-package :package1.package2.package1)
(defun foo ()
  "121")
(in-package :php)


(defpackage :package1.package2.package2
  (:use :cl)
  (:export #:foo))
(in-package :package1.package2.package2)
(defun foo ()
  "122")
(in-package :php)


(defpackage :package1.package2.package3
  (:use :cl)
  (:export #:foo))
(in-package :package1.package2.package3)
(defun foo ()
  "123")
(in-package :php)




(in-package :package1.package2.package3)
(funcall (php::myread "..package2:foo"))

(in-package :php)

