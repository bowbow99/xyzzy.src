;;; -*- mode: lisp -*-
;;;
;;; test/declare-type.l

#| Usage:
Just load this file.

    (load "path/to/declare-type.l")
    8 tests all passed.
    => t
|#

(defparameter *test-count* 0)

(defparameter *expected* nil)
(defparameter *actual* nil)

(defmacro expect-eql (expected form)
  ;; NOTE: Since we are modifying `let` and it's not reliable here,
  ;; avoid use of `let`.
  `(unwind-protect
       (progn
         (message "Testing... ~S" ',form)
         (setq *actual* ,form)
         (setq *expected* ,expected)
         (unless (eql *actual* *expected*)
           (error "Test failed: ~S~2%Expected:~%=> ~S~2%Actually:~%=> ~S~%"
                  ',form *expected* *actual*))
         (incf *test-count*))
     (setq *actual* '#:dummy
           *expected* '#:dummy)))

(defparameter *error* nil)

(defmacro expect-error (type form)
  `(unwind-protect
       (progn
         (message "Testing... ~S" ',form)
         (handler-case
             ,form
           (,type (e)
            (setq *error* e)))
         (if *error*
           (incf *test-count*)
           (error "Test failed: ~S~2%Expected: signal ~S~2%Actually: returned normally."
                  ',form ',type)))
     (setq *error* nil)))

#+xyzzy
(setf (get 'expect-eql 'ed:lisp-indent-hook) 1
      (get 'expect-error 'ed:lisp-indent-hook) 1)


(setq x :global-x)

(defun dynamic-x () x)

(defmacro check-global-x ()
  `(expect-eql :global-x x))


(progn  ; plain lexical binding
  (expect-eql 3
    (let ((x 3))
      x))
  (check-global-x)

  (expect-eql :global-x
    (let ((x 3))
      (dynamic-x)))
  (check-global-x))

(progn  ; special binding
  (expect-eql 3
    (let ((x 3))
      (declare (special x))
      x))
  (check-global-x)

  (expect-eql 3
    (let ((x 3))
      (declare (special x))
      (dynamic-x)))
  (check-global-x))

(progn  ; type checking on (declare (type TYPESPEC VAR))
  (expect-eql 3
    (let ((x 3))
      (declare (type fixnum x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (type string x))
      x))
  (check-global-x))

(progn  ; Not a type declaration nor a special declaration
  (expect-eql 3
    (let ((x 3))
      (declare (ignorable x))
      x))

  ;; NOTE: Parhaps this should signal an error or a warning. But no such
  ;; thing is implemented and just ignored ATM.
  (expect-eql 3
    (let ((x 3))
      (declare (ignore x))
      x))

  (expect-eql 3
    (let ((x 3))
      (declare (dynamic-extent x))
      x))

  (expect-eql 3
    (let ((x 3))
      (declare (optimize (speed 2) (safety 2) (debug 2)))
      x))

  (expect-eql 3
    (labels ((x () 3))
      (declare (ftype (function () fixnum) x))
      (x)))

  (expect-eql 3
    (labels ((x () 3))
      (declare (inline x))
      (x)))

  (expect-eql 3
    (labels ((x () 3))
      (declare (notinline x))
      (x))))

(progn  ; type checking on (declare (TYPESPEC VAR))
  (expect-eql 3
    (let ((x 3))
      (declare (fixnum x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (string x))
      x))
  (check-global-x))

(progn  ; type checking on (declare (type TYPESPEC VAR)) + special

  ;; in one `declare` expression
  (expect-eql 3
    (let ((x 3))
      (declare (special x)
               (type fixnum x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (special x)
               (type string x))
      x))
  (check-global-x)

  ;; in two separate `declare` expressions
  (expect-eql 3
    (let ((x 3))
      (declare (special x))
      (declare (type fixnum x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (special x))
      (declare (type string x))
      x))
  (check-global-x)

  (expect-eql 3
    (let ((x 3))
      (declare (type fixnum x))
      (declare (special x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (type string x))
      (declare (special x))
      x))
  (check-global-x))

(progn  ; type checking on (declare (TYPESPEC VAR)) + special

  ;; in one `declare` expression
  (expect-eql 3
    (let ((x 3))
      (declare (special x)
               (fixnum x))
      x))
  (check-global-x)

  (expect-eql 3
    (let ((x 3))
      (declare (fixnum x)
               (special x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (special x)
               (string x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (string x)
               (special x))
      x))
  (check-global-x)

  ;; in two separate `declare` expression
  (expect-eql 3
    (let ((x 3))
      (declare (special x))
      (declare (fixnum x))
      x))
  (check-global-x)

  (expect-eql 3
    (let ((x 3))
      (declare (fixnum x))
      (declare (special x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (special x))
      (declare (string x))
      x))
  (check-global-x)

  (expect-error type-error
    (let ((x 3))
      (declare (string x))
      (declare (special x))
      x))
  (check-global-x))


(format t "~D tests all passed.~%" *test-count*)
;;; test/declare-type.l ends here.
