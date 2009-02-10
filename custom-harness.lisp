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
   #:test-suite #:find-test #:run-test #:run-suite-test #:run-test-suite
   #:test-suite-error #:undefined-test-suite #:test-suite-failure #:test-suite-tests
   #:test-error #:unexpected-test-failure #:unexpected-test-success #:unexpected-test-compilation-success
   #:expected-test-runtime-error #:unexpected-test-runtime-lack-of-errors
   #:deftest #:deftest-expected-failure #:deftest-unstable-failure
   #:deftest-expected-compilation-failure #:deftest-expected-runtime-error
   #:subtest-success
   #:expect
   #:expect-value #:unexpected-value #:condition-expected #:condition-actual
   #:expect-success #:unexpected-failure #:condition-form
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

(defun report (stream format-control &rest format-args)
  (apply #'format stream format-control format-args)
  (finish-output stream))

(define-condition undefined-test-suite-test (test-suite-error)
  ((test :reader condition-test :initarg :test))
  (:report (lambda (cond stream)
             (report stream "~@<test suite ~S has no test ~S defined~:@>"
                     (condition-suite cond) (condition-test cond)))))

(define-condition test-suite-failure (test-suite-error)
  ((object :accessor condition-object :initarg :object)
   (conditions :accessor condition-conditions :type list :initarg :conditions))
  (:report (lambda (cond stream)
             (let (*print-length*)
               (report stream "~@<while running tests from suite ~S on ~S, following test errors were reported:~2I~_~{~T~W~_~}~:@>"
                       (condition-suite cond) (condition-object cond) (reverse (condition-conditions cond)))))))

(define-condition test-error (error)
  ((suite-name :accessor %condition-suite-name :initarg :suite-name)
   (test-name :accessor %condition-test-name :initarg :test-name)
   (subtest-id :accessor %condition-subtest-id :initarg :subtest-id)))

(defun test-error (type &rest args)
  (declare (special *suite-name* *test-name* *subtest-id*))
  (apply #'error type :suite-name *suite-name* :test-name *test-name* :subtest-id *subtest-id* args))

(define-condition unexpected-test-failure (test-error) ())
(define-condition expected-test-failure (test-error) ())
(define-condition unexpected-test-success (test-error)
  ()
  (:report (lambda (cond stream)
             (report stream "~@<unexpected success during test ~S~:@>"
                     (%condition-test-name cond)))))
(define-condition expected-test-compilation-failure (expected-test-failure)
  ()
  (:report (lambda (cond stream)
             (report stream "~@<expected failure compiling test ~S~:@>"
                     (%condition-test-name cond)))))
(define-condition unexpected-test-compilation-success (unexpected-test-success)
  ()
  (:report (lambda (cond stream)
             (report stream "~@<unexpected success compiling test ~S~:@>"
                     (%condition-test-name cond)))))
(define-condition expected-test-runtime-error (expected-test-failure)
  ((error :accessor condition-error :initarg :error))
  (:report (lambda (cond stream)
             (report stream "~@<expected runtime ~A ~A during test ~S~:@>"
                     (type-of (condition-error cond))
                     (condition-error cond)
                     (%condition-test-name cond)))))
(define-condition unexpected-test-runtime-lack-of-errors (unexpected-test-success)
  ((error-type :accessor condition-error-type :initarg :error-type))
  (:report (lambda (cond stream)
             (report stream "~@<unexpected lack of runtime errors of type ~S during test ~S~:@>"
                     (condition-error-type cond)
                     (%condition-test-name cond)))))

(define-condition unexpected-value (unexpected-test-failure)
  ((expected :accessor condition-expected :initarg :expected)
   (actual :accessor condition-actual :initarg :actual))
  (:report (lambda (cond stream)
             (let ((expected (condition-expected cond))
                   (actual (condition-actual cond))
                   (*print-base* 10))
               (report stream "~@<unexpected value during test ~S:~3I ~<expected: ~S, actual: ~S~:@>~:@>"
                       (condition-subtest-id cond) (list expected actual))))))

(defvar *test-suites* (make-hash-table))

(define-container-hash-accessor *test-suites* test-suite :coercer t)

(defun find-test (suite test &aux (suite (coerce-to-test-suite suite)))
  "Find TEST within SUITE."
  (find test (test-suite-tests suite) :key (compose #'car #'ensure-cons)))

(defun run-test (object test &optional (stream t))
  "Run TEST on OBJECT, reporting to STREAM, which defaults to T.

   TEST must be a test object, as returned by FIND-TEST."
  (lret ((result (returning-conditions test-error (funcall test object))))
	(report stream "~@<~A~:@>~%" result)))

(defun run-suite-test (object suite test &key (stream t) &aux (suite (coerce-to-test-suite suite)))
  "Run an individual TEST from test SUITE, passing it the OBJECT/
   Output is redirected to STREAM, defaulting to *ERROR-OUTPUT*.

   TEST muse be a name designating a test within SUITE."
  (if-let ((test (find-test suite test)))
    (run-test object test stream)
    (error 'undefined-test-suite-test :suite suite :test test)))

(defun run-test-suite (object suite &key (stream *standard-output*) (if-fail :continue)
                       &aux (suite (coerce-to-test-suite suite)))
  "Run all test functions in test SUITE, passing them the OBJECT.
   Output is redirected to STREAM, which defaults to *ERROR-OUTPUT*.

   When IF-FAIL is :ERROR, an error of type TEST-SUITE-FAILURE is raised upon test failures, 
   otherwise (the default), if it is :CONTINUE, NIL is returned in such cases.
   Failing all that, when all tests succeed, the value returned is T."
  (let ((*print-right-margin* 80)
        (tests (reverse (test-suite-tests suite)))
        conditions)
    #+ecl (report stream "~&---( ") ;; BUG: stream flushing in the context of P-P-L-B is broken in ECL.
    (pprint-logical-block (stream tests :prefix (format nil "running tests from suite ~S:" (name suite)))
      (terpri stream)
      (iter (for test-spec = (pprint-pop))
            (while test-spec)
            (destructuring-bind (test-name &key expected-failure unstable-failure)
                (ensure-cons test-spec)
              (let ((result (run-test object test-name stream)))
                (cond ((typep result 'test-error)
                       (if (or expected-failure unstable-failure
                               (typep result 'expected-test-failure))
                           (report stream "this failure is expected.~%")
                           (push result conditions)))
                      (expected-failure
                       (write
                        (first
                         (push (make-instance 'unexpected-test-success
                                :suite-name suite :test-name test-name) conditions))
                        :stream stream :escape nil)
                       (terpri stream)))))))
    (if conditions
        (ecase if-fail
          (:continue nil)
          (:error (error 'test-suite-failure :suite (name suite) :object object :conditions conditions)))
        t)))

(defmacro do-deftest (suite-name name lambda-list body &rest params &key &allow-other-keys)
  (declare (symbol suite-name))
  (multiple-value-bind (documentation declarations body) (destructure-def-body body)
    (with-gensyms (suite)
      `(let ((,suite (or (test-suite ',suite-name :if-does-not-exist :continue)
                         (setf (test-suite ',suite-name) (make-instance 'test-suite :name ',suite-name)))))
         (push ',(if params `(,name ,@params) name) (test-suite-tests ,suite))
         ,(with-defun-emission (name lambda-list :documentation documentation :declarations declarations)
           `(let ((*suite-name* ',suite-name) (*test-name* ',name) (*subtest-id* 0))
              (declare (special *suite-name* *test-name* *subtest-id*))
              ,@body))))))

(defmacro deftest (suite-name name lambda-list &body body)
  "Like DEFUN, but the defined function is associated with test suite SUITE-NAME."
  `(do-deftest ,suite-name ,name ,lambda-list (,@body)))

(defmacro deftest-expected-failure (suite-name name lambda-list &body body)
  "Like DEFTEST, but the success expectations are reversed."
  `(do-deftest ,suite-name ,name ,lambda-list (,@body) :expected-failure t))

(defmacro deftest-unstable-failure (suite-name name lambda-list &body body)
  "Like DEFTEST, both success and failure are ignored."
  `(do-deftest ,suite-name ,name ,lambda-list (,@body) :unstable-failure t))

(defmacro deftest-expected-compilation-failure (suite-name name lambda-list &body body)
  "Expect compilation of BODY with binding LAMBDA-LIST to fail."
  `(do-deftest ,suite-name ,name ,lambda-list
     ((declare (ignore ,@(lambda-list-binds lambda-list)))
      (if (ignore-errors (compile nil (lambda ,lambda-list ,@body)))
          (test-error 'unexpected-test-compilation-success)
          (test-error 'expected-test-compilation-failure)))))

(defmacro deftest-expected-runtime-error (error-type suite-name name lambda-list &body body)
  "Like DEFTEST, but catch unexpected runtime errors."
  (with-gensyms (rest)
    `(do-deftest ,suite-name ,name (,rest)
       ((if-let* ((error (nth-value 1 (ignore-errors (apply (lambda ,lambda-list ,@body) ,rest))))
                  (expected-error-p (typep error ',error-type)))
                 (test-error 'expected-test-runtime-error :error error)
                 (test-error 'unexpected-test-runtime-lack-of-errors :error-type ',error-type))))))

(defun subtest-success ()
  (declare (special *subtest-id*))
  (when *log-success*
    (report t "~@<subtest ~A passed~:@>~%" *subtest-id*))
  (incf *subtest-id*)
  t)

(defmacro expect (test-form (condition &rest condition-parameters) &body failure-body)
  "Expect TEST-FORM evaluate to non-NIL, otherwise executing FAILURE-BODY, and raising
   CONDITION with CONDITION-PARAMETERS passed."
  (unless (subtypep condition 'test-error)
    (error "~@<condition must be a subtype of CUSTOM-HARNESS:TEST-ERROR~:@>"))
  `(locally
     (cond (,test-form (subtest-success))
           (t
            ,@failure-body
            (test-error ',condition ,@condition-parameters)))))

(defun condition-subtest-id (cond)
  (list (%condition-suite-name cond) (%condition-test-name cond) (%condition-subtest-id cond)))

(define-condition unexpected-value (unexpected-test-failure)
  ((expected :accessor condition-expected :initarg :expected)
   (actual :accessor condition-actual :initarg :actual))
  (:report (lambda (cond stream)
             (let ((expected (condition-expected cond))
                   (actual (condition-actual cond))
                   (*print-base* 10))
               (report stream "~@<unexpected value during test ~S:~3I ~<expected: ~S, actual: ~S~:@>~:@>"
                       (condition-subtest-id cond) (list expected actual))))))

(defun expect-value (expected actual &key (test #'eql))
  "Expect ACTUAL value to match with what was EXPECTED, under TEST, defaulting to EQL.
   UNEXPECTED-VALUE is raised otherwise."
  (if (funcall test expected actual) (subtest-success)
      (test-error 'unexpected-value :expected expected :actual actual)))

(define-condition unexpected-failure (unexpected-test-failure)
  ((form :accessor condition-form :initarg :form))
  (:report (lambda (cond stream)
             (let ((*print-base* 10))
               (format stream "~@<unexpected NIL evaluation of form ~<~S~:@>~:@>"
                       (condition-form cond))))))

(defmacro expect-success (form)
  "Expect FORM evaluate to non-NIL, raising UNEXPECTED-FAILURE otherwise."
  `(if ,form
       (subtest-success)
       (test-error 'unexpected-failure :form ',form)))