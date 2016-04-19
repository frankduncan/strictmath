(defpackage #:strictmath (:use :common-lisp)
 (:shadow #:sin #:cos)
 (:export #:to-radians #:sin #:cos)
 (:documentation "Main strictmath package.

Strictmath is a pure common lisp implementation of Java's StrictMath class,
for only those functions as needed by CLNL to match NetLogo's output, and only
for doubles.  It should provide portable results for those functions that it
supports."))
