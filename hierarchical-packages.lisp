;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - hp.lisp
;; Description	     - cleaned-up hierarchical packages from acl 6
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Wed Aug 30 01:44:37 2000
;; Last Modified On  - Tue Apr 30 14:23:25 2002
;; Last Modified By  - Tim Bradshaw (tfb at lostwithiel)
;; Update Count	     - 8
;; Status	     - Unknown
;; 
;; $Id: //depot/www-tfeb-org/main/www-tfeb-org/html/programs/lisp/hierarchical-packages.lisp#1 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This code is based on that found at
;;; http://www.franz.com/support/documentation/6.0/doc/packages.htm.
;;; The following notice comes from the original file:
;;;
;;; The following source code is in the public domain.
;;; Provided "as is" with no warranty of any kind.  
;;; Use at your own risk.
;;;
;;; Despite the header above, I, Tim Bradshaw, am not the author of this
;;; code, I just changed it a bit.
;;;


#+allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (error "This code is not needed in Allegro!"))

(in-package :php)

;;; Should this PROVIDE stuff?

(pushnew :portable-hierarchical-packages *features*)

;;; Stash the original definition of CL:FIND-PACKAGE. Do this at
;;; compile time too, to stop the compiler complaining about unknown
;;; functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (fboundp 'real-find-package))
    (setf (symbol-function 'real-find-package)
	  (symbol-function 'cl:find-package))))

(define-condition hierarchical-package-error (package-error)
  ())

(define-condition simple-hierarchical-package-error
    (hierarchical-package-error simple-condition)
  ())

(declaim (inline ps->string))
(defun ps->string (ps)
  (declare (type (or package symbol string character) ps))
  (typecase ps
    (package (package-name ps))
    (symbol (symbol-name ps))
    (string ps)
    (character (string ps))))

(defun relative-package-name-to-package (name)
  ;; Given a package name, a string, do a relative package name lookup.
  ;;
  ;; It is intended that this function will be called from find-package.
  ;; In Allegro, find-package calls package-name-to-package, and the latter
  ;; function calls this function when it does not find the package.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare ;;(optimize speed)
           #+cmu
           (type simple-base-string name)
           #-cmu
           (type string name))
  (flet ((relative-to (package name)
           (declare (type string name))
           (if (string= "" name)
               package
               (real-find-package
                (concatenate 'simple-string
                             (package-name package) "." name))))
         (find-non-dot (name)
           #+cmu
           (declare (type simple-base-string name))
           #-cmu
           (declare (type string name))
           (do* ((len (length name))
                 (i 0 (1+ i)))
                ((= i len) nil)
             (declare (fixnum len i))
             (when (char/= #\. (char name i)) (return i)))))
    (when (char= #\. (char name 0))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to *package* name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((p *package*)
                     tmp)
                 (dotimes (i (1- n-dots))
                   (when (not (setq tmp (package-parent p)))
                     (error 'simple-hierarchical-package-error
                            :package p
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list p)))
                   (setq p tmp))
                 (relative-to p name))))))))

(defun package-parent (package-specifier)
  ;; Given package-specifier, a package, symbol or string, return the
  ;; parent package.  If there is not a parent, signal an error.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare ;;(optimize speed)
           (type (or package symbol string character) package-specifier))
  (flet ((find-last-dot (name)
           (declare (type string name))
           (do* ((len (1- (length name)))
                 (i len (1- i)))
                ((= i -1) nil)
             (declare (fixnum len i))
             (when (char= #\. (schar name i)) (return i)))))
    (let* ((child (ps->string package-specifier))
           (dot-position (find-last-dot child)))
      (cond (dot-position
             (let ((parent (subseq child 0 dot-position)))
               (or (real-find-package parent)
                   (error 'simple-hierarchical-package-error
                          :package child
                          :format-control "The parent of ~a does not exist."
                          :format-arguments (list child)))))
            (t (error 'simple-hierarchical-package-error
                      :package child
                      :format-control "There is no parent of ~a." 
                      :format-arguments (list child)))))))

(defun package-children (package-specifier &key (recurse t))
  ;; Given package-specifier, a package, symbol, character or string,
  ;; return all the packages which are in the hierarchy "under" the
  ;; given package.  If :recurse is nil, then only return the
  ;; immediate children of the package.
  ;;
  ;; While this function is not called via the reader, we do want it to be
  ;; fast.
  (declare ;;(optimize speed)
           (type (or package symbol string character) package-specifier))
  (let ((res ())
        (parent (ps->string package-specifier)))
    (labels
        ((string-prefix-p (prefix string)
           ;; Return length of `prefix' if `string' starts with `prefix'.
           ;; We don't use `search' because it does much more than we need
           ;; and this version is about 10x faster than calling `search'.
           (let ((prefix-len (length prefix))
                 (seq-len (length string)))
             (declare (fixnum prefix-len seq-len)
                      #+cmu
                      (type simple-base-string string))
             (when (>= prefix-len seq-len)
               (return-from string-prefix-p nil))
             (do* ((i 0 (1+ i)))
                  ((= i prefix-len) prefix-len)
               (declare (fixnum i))
               (when (not (char= (schar prefix i) (schar string i)))
                 (return nil)))))
         (test-package (package-name package)
           (let ((prefix
                  (string-prefix-p (concatenate 'simple-string parent ".")
                                   package-name)))
             (cond (recurse (when prefix (pushnew package res)))
                   (t (when (and prefix
                                 (not (find #\. package-name :start prefix)))
                        (pushnew package res)))))))

      ;; In Allegro, list-all-packages calls `sort', so use an internal
      ;; method to get the package names.
      #+allegro
      (maphash #'test-package *package-names*)
      #-allegro
      (dolist (package (list-all-packages))
        (funcall #'test-package (package-name package) package))
      
      res)))

;;; Per package aliases.  Nothing in this code actually defines these.
;;;

(defvar *per-package-alias-table*
  ;; maps from package -> alist of alias -> real names.
  ;; Lookups are nopt recursive in this list.
  ;; (could this be a constant?)
  (make-hash-table))

(defun find-hierarchical-package (name/package)
  ;;  (declare (optimize speed))            ;this is critical code
  (typecase name/package
    (package name/package)
    (t          ;should be STRINGable
     ;; PN is package name, EPN is effective (aliased) name
     ;; if there is one
     (let* ((pn (string name/package))
            (map (gethash *package* *per-package-alias-table*))
            (epn (and map (cdr (assoc pn map :test #'string=)))))
       ;; if there is an EPN, then do REAL-FIND-PACKAGE on it, 
       ;; otherwise use NAME/PACKAGE. not PN, in case it can do some
       ;; magic.  Otherwise look up a relative name.
       (or (real-find-package (or epn name/package))
           (relative-package-name-to-package (or epn pn)))))))
         

	 
	 
