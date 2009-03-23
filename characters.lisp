;;; constriction of character table and corresponding helpers are here
(in-package :php)

;; character class tests

(defun test-does-not-terminate-token (c) 
  (ignore-errors
    (return-from test-does-not-terminate-token
      (let* ((s (format nil "the-~A-symbol" c))
             (ss (concatenate 'string ":" s))
             (sym (intern s :keyword)))
        (eq sym 
            (dynamic-let1 (readtable-case *readtable*) :preserve
                          (read-from-string ss))))))
  nil)

(defun test-whitespace[2]p (c)
  (ignore-errors 
    (return-from test-whitespace[2]p 
      (equalp '(a b)
              (read-from-string (format nil "(a~Ab)" (the character c))))))
  nil)

(defun test-multiple-escape-p (c)
  (ignore-errors 
    (let1 good-symbol 
        (with-good-readtable 
            (dynamic-let1 (readtable-case *readtable*) :preserve
                          (read-from-string "qQ")))
      (return-from test-multiple-escape-p 
        (eq good-symbol 
            (dynamic-let1 (readtable-case *readtable*) :upcase
                          (read-from-string (format nil "~AqQ~A" c c)))))))
  nil)


(defun test-single-escape-p (c)
  (ignore-errors 
    (let1 good-symbol 
        (with-good-readtable 
            (read-from-string "\\'"))
      (return-from test-single-escape-p 
        (eq good-symbol 
            (read-from-string (format nil "~A'" c))))))
  nil)

;; constructing chartable

;; this will return character type based on previously defined tests
(defun get-char-type (c)
  "returns character type"
  (cond 
    ((member c '(#\.)) :dot) ; symbol names starting from these would be misinterpreted
    ((eql c #\:) :colon)           ; здесь тоже неплохо написать тест
    ((eql c #\() :open-brace)      ; здесь тоже неплохо написать тест
    ((test-does-not-terminate-token c) :does-not-terminate-token)
    ((test-whitespace[2]p c) :whitespace[2])
    ((test-multiple-escape-p c) :multiple-escape)
    ((test-single-escape-p c) :single-escape)))

;; character table itself
(defparameter *char-table*
  (loop for i from 0 to 256 for c = (code-char i) collecting (cons c (get-char-type c))))
