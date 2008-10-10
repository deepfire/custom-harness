;;; -*- Mode: Lisp -*-

(defpackage :custom-harness.system
  (:use :cl :asdf))

(in-package :custom-harness.system)

(defsystem :custom-harness
  :depends-on (:alexandria :iterate :pergamum)
  :components
  ((:file "custom-harness")))
