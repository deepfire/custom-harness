(defpackage custom-harness
  (:nicknames :cushar)
  (:use :common-lisp :alexandria :pergamum :iterate)
  (:export
   #:*log-success*
   #:test-suite #:run-test-suite #:test-suite-error #:undefined-test-suite #:test-suite-failure
   #:test-error #:test-failure
   #:deftest #:subtest-success #:expect #:unexpected-value #:condition-expected #:condition-actual #:expect-value
   #:*suite-name* #:*test-name* #:*subtest-id* #:condition-subtest-id))

(in-package :custom-harness)

(defparameter *log-success* nil)

(define-condition test-suite-error (error)
  ((suite :accessor condition-suite :initarg :suite)))

(define-condition undefined-test-suite (test-suite-error)
  ()
  (:report (lambda (cond stream)
             (format stream "~@<no tests defined for test suite ~S~:@>" (condition-suite cond)))))

(define-condition test-suite-failure (test-suite-error)
  ((object :accessor condition-object :initarg :object)
   (conditions :accessor condition-conditions :type list :initarg :conditions))
  (:report (lambda (cond stream)
             (let (*print-length*)
               (format stream "~@<while running tests from suite ~S on ~S, following test errors were reported:~2I~_~{~T~W~_~}~:@>"
                       (condition-suite cond) (condition-object cond) (reverse (condition-conditions cond)))))))

(defvar *test-suites* (make-hash-table))

(defun test-suite (name)
  (multiple-value-bind (value has-p) (gethash name *test-suites*)
    (unless has-p
      (error 'undefined-test-suite :suite name))
    value))

(defun run-test-suite (object suite-name &key (stream t))
  (lret ((*print-right-margin* 80) conditions (tests (reverse (test-suite suite-name)))
         (success t))
    (pprint-logical-block (stream tests :prefix (format nil "running tests from suite ~S:" suite-name))
      (terpri stream)
      (iter (for test = (pprint-pop))
            (while test)
            (format stream "~@<  ~A: ~A~:@>~%"
                    test (andf success (handler-case (funcall test object)
                                         (test-error (cond) (push cond conditions) cond))))))
    (andf success t) ;; Coerce to non-generalised boolean.
    (when conditions
      (error 'test-suite-failure :suite suite-name :object object :conditions conditions))))

(defmacro deftest (suite-name name lambda-list &body body)
  (declare (symbol suite-name))
  (multiple-value-bind (documentation declarations body) (destructure-def-body body)
    `(progn
       (pushnew ',name (gethash ,suite-name *test-suites*))
       ,(with-defun-emission (name lambda-list :documentation documentation :declarations declarations)
          `(let ((*suite-name* ,suite-name) (*test-name* ',name) (*subtest-id* 0))
             (declare (special *suite-name* *test-name* *subtest-id*))
             ,@body)))))

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
  (if (funcall test expected actual) (subtest-success)
      (test-error 'unexpected-value :expected expected :actual actual)))
