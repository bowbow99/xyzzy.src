;;; -*- Mode: Lisp; Package: SYSTEM -*-
;;;
;;; This file is part of xyzzy.
;;;

(in-package "lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(proclaim declaim))
  (export '(dynamic-extent ignore ignorable optimize ftype
            inline notinline))
  (export '(compilation-speed debug safety space speed)))

(in-package "system")

(defparameter *optimize-quality*
  '((speed . 2)
    (safety . 1)
    (debug . 0)
    (space . 1)
    (compilation-speed . 1)))

(defun proclaim (decl-spec)
  (case (car decl-spec)
    (special
     (mapc #'si:*make-special (cdr decl-spec)))
    (t nil)))

(defmacro declaim (&rest decl-specs)
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     ,@(mapcar (lambda (decl-spec)
                 `(proclaim ',decl-spec))
             decl-specs)))

(provide "declaration")
;;; declaration.l ends here.
