(in-package #:strictmath-test)

(defvar *tests* nil)

; This really is just here to check against regressions
(defun run-all-tests ()
 (let
  ((results (mapcar #'funcall (reverse *tests*))))
  (every #'identity results)))

(defun slurp-file (filename &key (element-type 'character) (sequence-type 'string))
 (with-open-file (str filename :element-type element-type)
  (let ((seq (make-sequence sequence-type (file-length str)))) (read-sequence seq str) seq)))

(defmacro deftest (name f)
 `(push
   (lambda ()
    (let
     ((success
       (handler-case
        (funcall ,f)
        (error (e) (format t "Got unexpected error in tests: ~A" e)))))
     (if success
      (format t "~c[1;32m- ~A passed~c[0m~%" #\Esc ,name #\Esc)
      (format t "~c[1;31m- ~A failed~c[0m~%" #\Esc ,name #\Esc))
     success))
   *tests*))

(deftest
 "To radians"
 (lambda ()
  (every
   #'identity
   (mapcar
    (lambda (pair)
     (destructuring-bind (deg expected-rad) pair
      (or
       (= (strictmath:to-radians deg) expected-rad)
       (format t "** Expected ~A but got ~A for ~A? **~%" expected-rad (strictmath:to-radians deg) deg))))
    (with-open-file (str "resources/testfiles/toRadiansData" :direction :input) (read str))))))

(deftest
 "sin"
 (lambda ()
  (every
   #'identity
   (mapcar
    (lambda (pair)
     (destructuring-bind (deg expected-sin) pair
      (or
       (= (strictmath:sin (strictmath:to-radians deg)) expected-sin)
       (format t "** Expected ~A but got ~A for ~A? **~%"
        expected-sin
        (strictmath:sin (strictmath:to-radians deg))
        deg))))
    (with-open-file (str "resources/testfiles/sinData" :direction :input) (read str))))))

(deftest
 "cos"
 (lambda ()
  (every
   #'identity
   (mapcar
    (lambda (pair)
     (destructuring-bind (deg expected-cos) pair
      (or
       (= (strictmath:cos (strictmath:to-radians deg)) expected-cos)
       (format t "** Expected ~A but got ~A for ~A? **~%"
        expected-cos
        (strictmath:cos (strictmath:to-radians deg))
        deg))))
    (with-open-file (str "resources/testfiles/cosData" :direction :input) (read str))))))
