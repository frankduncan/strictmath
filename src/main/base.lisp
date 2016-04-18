(in-package #:strictmath)

(defun to-radians (deg)
 "TO-RADIANS DEG => RAD

ARGUMENTS AND VALUES:

  DEG: A double representing the angle in degrees
  RAD: A double representing the angle in radians

DESCRIPTION:

  TO-RADIANS returns the radians equivalent of the angle passed in, in
  degrees."
 (* (/ deg 180d0) pi))

;(defun sin (rad)
; (cl:sin rad))
(defun decode-float64 (x)
 (let
  ((i (ieee-floats:encode-float64 x)))
  (values
   (ash i -32)
   (logand (ieee-floats:encode-float64 1.5707963267341256d0) #x00000000ffffffff))))

(defun encode-float64 (y1 y2)
 (ieee-floats:decode-float64 (+ (ash y1 32) y2)))

(defun float64-abs (x)
 (abs x))
