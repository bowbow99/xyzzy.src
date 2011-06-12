;;; -*- mode: lisp -*-
;;;
;;; test/structure-printer.l
;;;

#|
Usage:
just load this file. If something goes wrong, it signals an error and
stop. Otherwise, it prints "N tests all passed." message.

    (load "path/to/structure_printer.l")
    3 tests all passed.
    => t
|#

(defmacro expect-prints (form expected-output)
  `(let ((#1=#:actual-output
           (with-output-to-string (*standard-output*)
             ,form)))
     (unless (string= #1# ,expected-output)
       (error "Test failed: ~S~2%Expected:~%~A~2%Actually:~%~A"
              ',form ,expected-output #1#))
     (incf *test-count*)))
#+xyzzy
(setf (get 'expect-prints 'ed:lisp-indent-hook) 1)

(defparameter *test-count* 0)


;;;;
;;;; * Structure without :print-function

(defstruct foo x y)

(let ((foo (make-foo)))
  (expect-prints (princ foo)
    "#S(foo x nil y nil)")
  (expect-prints (prin1 foo)
    "#S(foo x nil y nil)"))

(let ((foo (make-foo :x 1 :y "hi")))
  (expect-prints (princ foo)
    "#S(foo x 1 y hi)")
  (expect-prints (prin1 foo)
    "#S(foo x 1 y \"hi\")"))

;;;;
;;;; * Structure with :print-function

(defstruct (bar (:print-function (lambda (o s n)
                                   (format s "#<bar ~S ~S>"
                                     (bar-x o)
                                     (bar-y o)))))
  x y)

(let ((bar (make-bar)))
  (expect-prints (princ bar)
    "#<bar nil nil>")
  (expect-prints (prin1 bar)
    "#<bar nil nil>"))

(let ((bar (make-bar :x 1 :y :hello)))
  (expect-prints (princ bar)
    "#<bar 1 :hello>")
  (expect-prints (prin1 bar)
    "#<bar 1 :hello>"))

;;;;
;;;; * Condition without :report

(let ((err (make-condition 'error)))
  (expect-prints (princ err)
    "#S(error)")
  (expect-prints (prin1 err)
    "#S(error)"))

;;;;
;;;; * Condition with :report

(let ((err (make-condition 'simple-error :format-string "É_ÉÅÇ≈Ç∑: ~S" :format-arguments '(3))))
  (expect-prints (princ err)
    "É_ÉÅÇ≈Ç∑: 3")
  (expect-prints (prin1 err)
    "#S(simple-error format-string \"É_ÉÅÇ≈Ç∑: ~S\" format-arguments (3))"))



(format t "~S tests all passed.~%" *test-count*)

;;; test/structure-printer.l ends here.
