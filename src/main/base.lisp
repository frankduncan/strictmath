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
