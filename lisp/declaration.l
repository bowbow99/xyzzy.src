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

(defun proclaim (decl-spec)
  (when (eq (car decl-spec) 'special)
    (mapc #'(lambda (x)
              (si:*make-special x))
          (cdr decl-spec))))

(defmacro declaim (&rest decl-specs)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cons 'progn
          (mapcan #'(lambda (decl-spec)
                      (when (eq (car decl-spec) 'special)
                        (mapcar #'(lambda (x) `(defvar ,x))
                          (cdr decl-spec))))
              decl-specs))))


(provide "declaration")
;;; declaration.l ends here.
