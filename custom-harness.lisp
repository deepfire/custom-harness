;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALPINE; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(defpackage custom-harness
  (:nicknames :cushar)
  (:use :common-lisp :alexandria :pergamum :iterate)
  (:export
   #:*log-success*
   #:test-suite #:run-suite-test #:run-test-suite #:test-suite-error #:undefined-test-suite #:test-suite-failure #:test-suite-tests
   #:test-error #:test-failure
   #:deftest #:subtest-success #:expect #:unexpected-value #:unexpected-failure
   #:condition-expected #:condition-actual #:expect-value #:expect-success
   #:*suite-name* #:*test-name* #:*subtest-id* #:condition-subtest-id))

(in-package :custom-harness)

(defparameter *log-success* nil)

(defclass test-suite ()
  ((name :accessor name :initarg :name)
   (tests :accessor test-suite-tests :type list :initform nil :initarg :tests)))

(define-condition test-suite-error (error)
  ((suite :accessor condition-suite :initarg :suite)))

(define-condition undefined-test-suite (test-suite-error)
  ()
  (:report (lambda (cond stream)
             (format stream "~@<no tests defined for test suite ~S~:@>" (condition-suite cond)))))

(define-condition undefined-test-suite-test (test-suite-error)
  ((test :reader condition-test :initarg :test))
  (:report (lambda (cond stream)
             (format stream "~@<test suite ~S has no test ~S defined~:@>"
                     (condition-suite cond) (condition-test cond)))))

(define-condition test-suite-failure (test-suite-error)
  ((object :accessor condition-object :initarg :object)
   (conditions :accessor condition-conditions :type list :initarg :conditions))
  (:report (lambda (cond stream)
             (let (*print-length*)
               (format stream "~@<while running tests from suite ~S on ~S, following test errors were reported:~2I~_~{~T~W~_~}~:@>"
                       (condition-suite cond) (condition-object cond) (reverse (condition-conditions cond)))))))

(defvar *test-suites* (make-hash-table))

(define-container-hash-accessor *test-suites* test-suite :coercer t)

(defun do-run-suite-test (object test stream)
  (lret ((result (returning-conditions test-error (funcall test object))))
    (format stream "~@<  ~A: ~A~:@>~%" test result)))

(defun run-suite-test (object suite test &key (stream t) &aux (suite (coerce-to-test-suite suite)))
  (unless (find test (test-suite-tests suite))
    (error 'undefined-test-suite-test :suite suite :test test))
  (do-run-suite-test object test stream))

(defun run-test-suite (object suite &key (stream t) &aux (suite (coerce-to-test-suite suite)))
  (lret ((*print-right-margin* 80) conditions (tests (reverse (test-suite-tests suite)))
         (success t))
    (pprint-logical-block (stream tests :prefix (format nil "running tests from suite ~S:" (name suite)))
      (terpri stream)
      (iter (for test = (pprint-pop)) (while test) 
            (let ((result (do-run-suite-test object test stream)))
              (when (typep result 'test-error) (collect result into conditions)))))
    (andf success t) ;; Coerce to non-generalised boolean.
    (when conditions
      (error 'test-suite-failure :suite (name suite) :object object :conditions conditions))))

(defmacro deftest (suite-name name lambda-list &body body)
  (declare (symbol suite-name))
  (multiple-value-bind (documentation declarations body) (destructure-def-body body)
    (with-gensyms (suite)
      `(let ((,suite (or (test-suite ',suite-name :if-does-not-exist :continue)
                         (setf (test-suite ',suite-name) (make-instance 'test-suite :name ',suite-name)))))
         (pushnew ',name (test-suite-tests ,suite))
         ,(with-defun-emission (name lambda-list :documentation documentation :declarations declarations)
           `(let ((*suite-name* ',suite-name) (*test-name* ',name) (*subtest-id* 0))
              (declare (special *suite-name* *test-name* *subtest-id*))
              ,@body))))))

(defun subtest-success ()
  (declare (special *subtest-id*))
  (when *log-success*
    (format t "~@<subtest ~A passed~:@>~%" *subtest-id*))
  (incf *subtest-id*)
  t)

(define-condition test-error (error)
  ((suite-name :accessor %condition-suite-name :initarg :suite-name)
   (test-name :accessor %condition-test-name :initarg :test-name)
   (subtest-id :accessor %condition-subtest-id :initarg :subtest-id)))

(defun test-error (type &rest args)
  (declare (special *suite-name* *test-name* *subtest-id*))
  (apply #'error type :suite-name *suite-name* :test-name *test-name* :subtest-id *subtest-id* args))

(defun condition-subtest-id (cond)
  (list (%condition-suite-name cond) (%condition-test-name cond) (%condition-subtest-id cond)))

(define-condition test-failure (test-error)
  ())

(defmacro expect (test-form (condition &rest condition-parameters) &body failure-body)
  (unless (subtypep condition 'test-error)
    (error "~@<condition must be a subtype of CUSTOM-HARNESS:TEST-ERROR~:@>"))
  `(locally
     (cond (,test-form (subtest-success))
           (t
            ,@failure-body
            (test-error ',condition ,@condition-parameters)))))

(define-condition unexpected-value (test-failure)
  ((expected :accessor condition-expected :initarg :expected)
   (actual :accessor condition-actual :initarg :actual))
  (:report (lambda (cond stream)
             (let ((expected (condition-expected cond))
                   (actual (condition-actual cond))
                   (*print-base* 10))
               (format stream "~@<unexpected value during test ~S:~3I ~<expected: ~S, actual: ~S~:@>~:@>"
                       (condition-subtest-id cond) (list expected actual))))))

(defun expect-value (expected actual &key (test #'eql))
  "Expect ACTUAL value to match with what was EXPECTED, under TEST, defaulting to EQL.
   UNEXPECTED-VALUE is raised otherwise."
  (if (funcall test expected actual) (subtest-success)
      (test-error 'unexpected-value :expected expected :actual actual)))

(define-condition unexpected-failure (test-failure)
  ((form :accessor condition-form :initarg :form))
  (:report (lambda (cond stream)
             (let ((*print-base* 10))
               (format stream "~@<unexpected NIL evaluation of form ~<~S~:@>~:@>"
                       (condition-form cond))))))

(defmacro expect-success (form)
  "Expect FORM evaluate to non-NIL, raising UNEXPECTED-FAILURE otherwise."
  `(unless ,form
     (test-error 'unexpected-failure :form ',form)))