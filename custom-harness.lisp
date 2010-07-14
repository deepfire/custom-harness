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
   #:*test-critical-features*
   #:*log-success*
   #:test-featurep
   #:test-suite
   #:find-test
   #:run-test
   #:run-suite-test
   #:run-test-suite
   #:test-suite-error
   #:undefined-test-suite
   #:test-suite-failure
   #:test-suite-tests
   #:test-error
   #:signal-test-error
   #:simple-test-error
   #:unexpected-test-failure
   #:unexpected-test-success
   #:unexpected-test-compilation-success
   #:expected-test-runtime-error
   #:unexpected-test-runtime-lack-of-errors
   #:deftest
   #:deftest-expected-failure
   #:deftest-unstable-failure
   #:deftest-expected-compilation-failure
   #:deftest-expected-runtime-error
   #:with-subtest
   #:condition-subtest-critical-p
   #:current-subtest
   #:expect
   #:expect-value
   #:unexpected-value
   #:condition-expected
   #:condition-actual
   #:expect-success
   #:unexpected-failure
   #:condition-form
   #:*suite-name*
   #:*test-name*
   #:condition-subtest-id))

(in-package :custom-harness)

(defvar *test-critical-features* nil)
(defparameter *log-success* nil)

(defun test-critical-featurep (feature)
  "See if test-related FEATURE is present."
  (find feature *test-critical-features* :test #'eq))

(defclass test-suite ()
  ((name :accessor name :initarg :name)
   (tests :accessor test-suite-tests :type list :initform nil :initarg :tests)))

(define-condition test-suite-error (error)
  ((suite :accessor condition-suite :initarg :suite)))

(define-reported-condition undefined-test-suite (test-suite-error)
  ()
  (:report (suite) "~@<No tests defined for test suite ~S.~:@>" suite))

(define-reported-condition undefined-test-suite-test (test-suite-error)
  ((test :reader condition-test :initarg :test))
  (:report (test suite) "~@<Test suite ~S has no test ~S defined.~:@>" suite test))

(define-reported-condition test-suite-failure (test-suite-error)
  ((object :accessor condition-object :initarg :object)
   (conditions :accessor condition-conditions :type list :initarg :conditions))
  (:report (suite object conditions)
           "~@<While running tests from suite ~S on ~S, following test errors were reported:~2I~_~{~T~W~_~}~:@>"
           suite object (reverse conditions)))

(define-condition test-error (error)
  ((suite-name :accessor condition-suite-name :initarg :suite-name)
   (test-name :accessor condition-test-name :initarg :test-name)
   (subtest-id :accessor %condition-subtest-id :initarg :subtest-id)
   (critical-p :accessor condition-subtest-critical-p :initarg :critical-p)))

(define-condition simple-test-error (test-error simple-error)
  ()
  (:report report-simple-condition))

(defun signal-test-error (type &rest args)
  (declare (special *suite-name* *test-name* *subtest-id* *subtest-not-critical*))
  (let ((condition (apply #'make-condition type :suite-name *suite-name* :test-name *test-name* :subtest-id *subtest-id*
                          :critical-p (not *subtest-not-critical*)
                          args)))
    (if *subtest-not-critical*
        (format *error-output* "~A~%" condition)
        (error condition))))

(defun current-subtest ()
  (locally (declare (special *subtest-id*))
    *subtest-id*))

(defun test-error (format-control &rest format-arguments)
  (signal-test-error 'simple-test-error :format-control format-control :format-arguments format-arguments))

(define-condition unexpected-test-failure (test-error) ())
(define-condition expected-test-failure (test-error) ())
(define-reported-condition unexpected-test-success (test-error)
  ()
  (:report (test-name) "~@<Unexpected success during test ~S~:@>" test-name))
(define-reported-condition expected-test-compilation-failure (expected-test-failure)
  ()
  (:report (test-name) "~@<Expected failure compiling test ~S~:@>" test-name))
(define-reported-condition unexpected-test-compilation-success (unexpected-test-success)
  ()
  (:report (test-name) "~@<Unexpected success compiling test ~S~:@>" test-name))
(define-reported-condition expected-test-runtime-error (expected-test-failure)
  ((error :accessor condition-error :initarg :error))
  (:report (error test-name) "~@<Expected runtime ~A ~A during test ~S~:@>" (type-of error) error test-name))
(define-reported-condition unexpected-test-runtime-lack-of-errors (unexpected-test-success)
  ((error-type :accessor condition-error-type :initarg :error-type))
  (:report (error-type test-name) "~@<Unexpected lack of runtime errors of type ~S during test ~S~:@>" error-type test-name))

(define-reported-condition unexpected-value (unexpected-test-failure)
  ((expected :accessor condition-expected :initarg :expected)
   (actual :accessor condition-actual :initarg :actual))
  (:report (expected actual subtest-id) "~@<Unexpected value during test ~A:~3I ~<expected: ~X, actual: ~X~:@>~:@>" subtest-id (list expected actual)))

(defvar *test-suites* (make-hash-table))

(define-root-container *test-suites* test-suite :coercer t)

(defun find-test (suite test &aux (suite (coerce-to-test-suite suite)))
  "Find TEST within SUITE."
  (find test (test-suite-tests suite) :key (compose #'car #'ensure-cons)))

(defun run-test (object test &optional (stream *standard-output*))
  "Run TEST on OBJECT, reporting to STREAM, which defaults to T.

   TEST must be a test object, as returned by FIND-TEST."
  (syncformat stream "    ~A: " test)
  (multiple-value-bind (condition backtrace test-result) (with-collected-conditions (test-error)
                                                           (funcall test object))
    (declare (ignore backtrace))
    (syncformat stream (if test-result
                           "~50,25T~A~%"
                           "~%~7T~A~%")
                (or test-result condition))
    (or test-result condition)))

(defun run-suite-test (object suite test &key (stream *standard-output*) &aux (suite (coerce-to-test-suite suite)))
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
        expected-failures
        conditions)
    #+ecl (syncformat stream "~&---( ") ;; BUG: stream flushing in the context of P-P-L-B is broken in ECL.
    (pprint-logical-block (stream tests :prefix (format nil "running tests from suite ~S:" (name suite)))
      (terpri stream)
      (iter (for test-spec = (pprint-pop))
            (while test-spec)
            (destructuring-bind (test-name &key expected-failure unstable-failure) (ensure-cons test-spec)
              (let ((result (run-test object test-name stream)))
                (cond ((typep result 'test-error)
                       (if (or expected-failure unstable-failure
                               (typep result 'expected-test-failure))
                           (progn (syncformat stream "this failure is expected.~%")
                                  (push result expected-failures))
                           (push result conditions)))
                      (expected-failure
                       ;; There was no error where one was expected, NO GOOD!
                       (write (first (push (make-instance 'unexpected-test-success :suite-name suite :test-name test-name)
                                           conditions))
                              :stream stream :escape nil)
                       (terpri stream)
                       (finish-output stream)))))))
    (if conditions
        (ecase if-fail
          (:continue (values nil expected-failures conditions))
          (:error (error 'test-suite-failure :suite (name suite) :object object :conditions conditions)))
        (values t expected-failures))))

(defmacro do-deftest (suite-name name required-features lambda-list body &rest params &key &allow-other-keys)
  (declare (symbol suite-name))
  (multiple-value-bind (documentation declarations body) (destructure-def-body body)
    (let ((aggregate-params (append params (when required-features `(:required-features ,required-features)))))
      (with-gensyms (suite)
        `(let ((,suite (or (test-suite ',suite-name :if-does-not-exist :continue)
                           (setf (test-suite ',suite-name) (make-instance 'test-suite :name ',suite-name)))))
           (unless (find ',name (mapcar #'ensure-cons (test-suite-tests ,suite)) :key #'car)
             (push ',(if aggregate-params `(,name ,@aggregate-params) name) (test-suite-tests ,suite)))
           ,(with-defun-emission (name lambda-list :documentation documentation :declarations declarations)
              `(let ((*suite-name* ',suite-name) (*test-name* ',name) *subtest-id* *subtest-not-critical*)
                 (declare (special *suite-name* *test-name* *subtest-id* *subtest-not-critical*))
                 ,@body)))))))

(defmacro deftest (suite-name name required-features lambda-list &body body)
  "Like DEFUN, but the defined function is associated with test suite SUITE-NAME.

   The test is only made available if all of REQUIRED-FEATURES are also present
   in *TEST-CRITICAL-FEATURES* during runtime."
  `(do-deftest ,suite-name ,name ,required-features ,lambda-list (,@body)))

(defmacro deftest-expected-failure (suite-name name required-features lambda-list &body body)
  "Like DEFTEST, but the success expectations are reversed.

   The test is only made available if all of REQUIRED-FEATURES are also present
   in *TEST-CRITICAL-FEATURES* during runtime."
  `(do-deftest ,suite-name ,name ,required-features ,lambda-list (,@body) :expected-failure t))

(defmacro deftest-unstable-failure (suite-name name required-features lambda-list &body body)
  "Like DEFTEST, both success and failure are ignored.

   The test is only made available if all of REQUIRED-FEATURES are also present
   in *TEST-CRITICAL-FEATURES* during runtime."
  `(do-deftest ,suite-name ,name ,required-features ,lambda-list (,@body) :unstable-failure t))

(defmacro deftest-expected-compilation-failure (suite-name name required-features lambda-list &body body)
  "Expect compilation of BODY with binding LAMBDA-LIST to fail.

   The test is only made available if all of REQUIRED-FEATURES are also present
   in *TEST-CRITICAL-FEATURES* during runtime."
  `(do-deftest ,suite-name ,name ,required-features ,lambda-list
     ((declare (ignore ,@(lambda-list-binds lambda-list)))
      (signal-test-error (if (ignore-errors (compile nil (lambda ,lambda-list ,@body)))
                             'unexpected-test-compilation-success
                             'expected-test-compilation-failure)))))

(defmacro deftest-expected-runtime-error (error-type suite-name name required-features lambda-list &body body)
  "Like DEFTEST, but catch unexpected runtime errors.

   The test is only made available if all of REQUIRED-FEATURES are also present
   in *TEST-CRITICAL-FEATURES* during runtime."
  (with-gensyms (rest)
    `(do-deftest ,suite-name ,name ,required-features (,rest)
       ((if-let* ((error (nth-value 1 (ignore-errors (apply (lambda ,lambda-list ,@body) ,rest))))
                  (expected-error-p (typep error ',error-type)))
                 (signal-test-error 'expected-test-runtime-error :error error)
                 (signal-test-error 'unexpected-test-runtime-lack-of-errors :error-type ',error-type))))))

(defmacro with-subtest (name &body body)
  (destructuring-bind (name &key noncritical-p) (ensure-cons name)
    `(let ((*subtest-id* ,name)
           ,@(when noncritical-p `((*subtest-not-critical* t))))
       (declare (special *subtest-id* ,@(when noncritical-p `(*subtest-not-critical*))))
       ,@body)))

(defmacro expect (test-form (condition &rest condition-parameters) &body failure-body)
  "Expect TEST-FORM evaluate to non-NIL, otherwise executing FAILURE-BODY, and raising
   CONDITION with CONDITION-PARAMETERS passed."
  (unless (subtypep condition 'test-error)
    (error "~@<condition must be a subtype of CUSTOM-HARNESS:TEST-ERROR~:@>"))
  `(locally
     (cond (,test-form t)
           (t
            ,@failure-body
            (signal-test-error ',condition ,@condition-parameters)))))

(defun condition-subtest-id (cond)
  (list* (condition-suite-name cond) (condition-test-name cond) (xform-if #'identity #'list (%condition-subtest-id cond))))

(defun expect-value (expected actual &key (test #'eql))
  "Expect ACTUAL value to match with what was EXPECTED, under TEST, defaulting to EQL.
   UNEXPECTED-VALUE is raised otherwise."
  (if (funcall test expected actual)
      t
      (signal-test-error 'unexpected-value :expected expected :actual actual)))

(define-condition unexpected-failure (unexpected-test-failure)
  ((form :accessor condition-form :initarg :form))
  (:report (lambda (cond stream)
             (let ((*print-base* 10))
               (format stream "~@<form ~<~S~:@> unexpectedly evaluated to NIL~:@>"
                       (condition-form cond))))))

(defmacro expect-success (form)
  "Expect FORM evaluate to non-NIL, raising UNEXPECTED-FAILURE otherwise."
  `(or ,form
       (signal-test-error 'unexpected-failure :form ',form)))