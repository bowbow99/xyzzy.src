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

(defvar *ignorable-declarations* nil
  "List of declaration identifiers those are declared as `declaration`.")

(defun register-declaration-identifier (identifiers)
  (dolist (id identifiers)
    (unless (symbolp id)
      (error 'type-error :datum id :expected-type 'symbol))
    (pushnew id *ignorable-declarations*)))

(defparameter *optimize-quality*
  '((speed . 2)
    (safety . 1)
    (debug . 0)
    (space . 1)
    (compilation-speed . 1)))

(defun set-optimize-quality (decl)
  (multiple-value-bind (key value)
      (cond ((symbolp decl) (values decl 3))
            ((consp decl)   (values (car decl) (cadr decl)))
            (t (error 'type-error :datum decl :expected-type '(or symbol cons))))
    (unless (member key '(speed safety debug space compilation-speed))
      (error "知らない最適化フラグっす: ~S" key))
    (unless (and (integerp value)
                 (<= 0 value 3))
      (error "不正な最適化の値っす: (~S ~S)" key value))
    (setf (cdr (assoc key *optimize-quality*)) value)))

(defun proclaim (decl-spec)
  (let ((identifier (car decl-spec))
        (do-nothing (lambda (x) (declare (ignore x)))))
    (cons identifier
          (mapc (case identifier
                  (special #'si:*make-special)
                  (optimize #'set-optimize-quality)
                  (declaration #'register-declaration-identifier)
                  ((dynamic-extent ignore ignorable)
                   (warn "グローバルに `~S' はダメっす" identifier)
                   do-nothing)
                  (t
                   (unless (member identifier *ignorable-declarations*)
                     (warn "Unknown/unsupported declaration: ~S" identifier))
                   do-nothing))
                (cdr decl-spec)))))

(defmacro declaim (&rest decl-specs)
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     ,@(mapcar (lambda (decl-spec)
                 `(proclaim ',decl-spec))
             decl-specs)))

(provide "declaration")
;;; declaration.l ends here.
