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
         (unless *error*
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




(format t "~D tests all passed.~%" *test-count*)
;;; test/declare-type.l ends here.
