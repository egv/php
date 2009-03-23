(in-package :php)

(defun unread-char* (char stream) "Возвращает новый stream"
  (unread-char char stream)
  stream
  #+nil (make-concatenated-stream (make-string-input-stream (concatenate 'string (list char))) stream))

(defun find-by-designator (sss)
  (if (symbolp sss) (find-symbol (symbol-name sss)) sss))

(defun interpret-compound-token (stream token)
  "Разбирает токен, прочитанный из read-token-with-colons и начинающийся с char"
  (cond 
    ((null token) token) ;; например, #-
    (t
     (case (length token)
       (1 (find-by-designator (car token)))
       (3 (let1 2nd  (second token)
            (or (and (consp 2nd)
                     (eq (car 2nd) 'colons)) 
                (simple-reader-error stream "Strange compound token ~S" token))
            (find-symbol
             (symbol-name (find-by-designator (third token))) 
             (find-hierarchical-package (first token)))))
       (t (simple-reader-error stream "Strange compound token ~S" token))))))

(defun careful-token-reader (stream char) 
  (interpret-compound-token stream (read-token-with-colons stream char)))

(defun starting-colon-reader (stream char)
;  (format t "ungething COLON and reading as usual")
  (setf stream (unread-char* char stream))
  (with-good-readtable
    (read stream t nil t)))
  

(defun simple-reader-error (stream format-string &rest args )
  (error "~A in stream ~A" (apply 'format nil format-string args) stream))

(defparameter *colon-readtable* (copy-readtable nil))

(set-syntax-from-char #\: #\  *colon-readtable* *colon-readtable*)

(defun read-token-with-colons (stream char)
  "читает токен, при этом двоеточия являются временными разделителями. Мы должны находиться в readtable, где
colon прерывает чтение. Мы стоим на не белой букве"
  (print char)
  (setf stream (unread-char* char stream))
  (when *read-suppress*
    (return-from read-token-with-colons (with-good-readtable (read stream))))
  (let ((*readtable* *colon-readtable*)
        (result nil))
    (with-foo-package
        (setf result 
              (iter outer
                    (flet ((coll (x) (:collect x :into result)))
                      (:for tok :next (read-preserving-whitespace stream nil nil))
                      (unless tok (return-from outer result)) ; на первой итерации, наверное, этого не может случиться
                      (coll tok)
                      (iter 
                        (:for cnt :from 0)
                        (:for c :next (read-char stream nil nil))
                        (unless c (return-from outer result))
                        (:for type = (cdr (assoc c *char-table*)))
                        (case type
                          (:colon
                           (when (and (> cnt 2) (not *read-suppress*)) (simple-reader-error stream "To many colons in ~S" result)))
                          ((:does-not-terminate-token :dot)
                           (setf stream (unread-char* c stream))
                           (coll `(colons ,@cnt))
                           (:leave))
                          (t 
                           (cond 
                             ((:first-time-p)
                              (unread-char c stream) 
                              (return-from outer result))
                             (t 
                              (simple-reader-error stream "Wrong compound token ~S" result)))))))))
        (unless *read-suppress* result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable ;;;;;;;;;;;;;
(setf *readtable* (copy-readtable nil))
(setf *my-readtable* (copy-readtable nil))
(setf *good-readtable* (copy-readtable nil))

(iter:iter 
 (:for i :in *char-table*)
 (:for c := (car i))
 (:for b := (cdr i))
 (ecase b 
   ((:dot :does-not-terminate-token :multiple-escape)
    (set-syntax-from-char c #\# *my-readtable* *good-readtable*) ; will make it non-terminating macro character
    (set-macro-character c 'careful-token-reader t *my-readtable*))
   (:colon
    (set-macro-character c 'starting-colon-reader t *my-readtable*))
   ((:single-escape :open-brace :whitespace[2] nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable end ;;;;;;;;;;;;;
(defun myread (s) (with-my-readtable (read-from-string s)))

(defun open-bracket-macro-character (stream char)
  (print char)
  `',(read-delimited-list #\] stream t))

(set-macro-character #\[ #'open-bracket-macro-character nil *my-readtable*)
(set-macro-character #\] (get-macro-character #\)) nil *my-readtable*)