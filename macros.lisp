;;; different utility macros and paramaters are defined in this file

(in-package :php)

;; makes it  
;; (let ((var val))
;;   ( ... body ... ))
(defmacro let1 (var val &body body) `(let ((,var ,val)) ,@body))

(defmacro dynamic-let1 (place-form value &body body)
  "вычисляем place-form и запоминаем его значение.
Присваиваем ему value. Выполянем body как progn, на выходе 
вычисляем place-form и записываем value, которое мы запомнили"
  (cl-utilities::with-gensyms 
   (saved-value)
   `(let1 ,saved-value ,place-form
      (setf ,place-form ,value)
      (unwind-protect
          (progn ,@body)
        (setf ,place-form ,saved-value)))))

;;; readtable-related macros
(defparameter *good-readtable* (copy-readtable nil) "Sample initial readtable for tests")
(defparameter *my-readtable* (copy-readtable nil) "Readtable for use with see-packages")

(defmacro with-readtable (readtable &body body)
  "evaluates body with *readtable* set to readtable"
  `(let1 *readtable* ,readtable ,@body))

(defmacro with-my-readtable (&body body) 
  "evaluates body with readtable set to *my-readtable*"
  `(with-readtable *my-readtable* ,@body))

(defmacro with-good-readtable (&body body) 
  "evaluates body with readtable set to *good readtable*"
  `(with-readtable *good-readtable* ,@body))

(defpackage :foo-package (:use)) 

(defmacro with-foo-package (&body body) 
  `(let1 *package* ,(find-package :foo-package) 
         #+ignore (loop for s being the symbols in ,(find-package :foo-package) do (unintern s))
     ,@body))

