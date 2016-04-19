(in-package #:strictmath)
; This file is taken from part of Evita Common Lisp.
;
; Copyright (C) 1996-2007 by Project Vogue.
; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;
; Before that, it was based off of fdlibm
;
;  See fdlibm (http://www.netlib.org/fdlibm/)
;  See http://sources.redhat.com/newlib/
; 
; /* @(#)s_cos.c 5.1 93/09/24 */
; /*
;  * ====================================================
;  * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
;  *
;  * Developed at SunPro, a Sun Microsystems, Inc. business.
;  * Permission to use, copy, modify, and distribute this
;  * software is freely granted, provided that this notice 
;  * is preserved.
;  * ====================================================
;  */
; 
; /* cos(x)
;  * Return cosine function of x.
;  *
;  * kernel function:
;  *      __kernel_sin                ... sine function on [-pi/4,pi/4]
;  *      __kernel_cos                ... cosine function on [-pi/4,pi/4]
;  *      __ieee754_rem_pio2        ... argument reduction routine
;  *
;  * Method.
;  *      Let S,C and T denote the sin, cos and tan respectively on 
;  *      [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
;  *      in [-pi/4 , +pi/4], and let n = k mod 4.
;  *      We have
;  *
;  *          n        sin(x)      cos(x)        tan(x)
;  *     ----------------------------------------------------------
;  *          0               S           C                 T
;  *          1               C          -S                -1/T
;  *          2              -S          -C                 T
;  *          3              -C           S                -1/T
;  *     ----------------------------------------------------------
;  *
;  * Special cases:
;  *      Let trig be any of sin, cos, or tan.
;  *      trig(+-INF)  is NaN, with signals;
;  *      trig(NaN)    is that NaN;
;  *
;  * Accuracy:
;  *      TRIG(x) returns trig(x) nearly rounded 
;  */

(defun cos (x)
 "COS X => RESULT

ARGUMENTS AND VALUES:

  X: A double representing the angle in radians bounded from [0, 2pi]
  RESULT: A double representing the cos of the angle

DESCRIPTION:

  COS returns the cos of the angle X."
 (declare (values double-float))
 (declare (type double-float x))
 (let ((hx (logand (decode-float64 x) #x7fffffff)))
  (cond
   ;; |x| ~< pi/4
   ((<= hx #x3fe921fb) (float64-kernel-cos x 0d0))

   ;; cos(Inf or NaN) is NaN
   ((>= hx #x7ff00000) (- x x))

   ;; argument reduction needed
   (t
    (multiple-value-bind (n y0 y1) (float64-rem-pio2 x)
     (ecase (logand n 3)
      (0 (float64-kernel-cos y0 y1))
      (1 (- (float64-kernel-sin y0 y1 1)))
      (2 (- (float64-kernel-cos y0 y1)))
      (3 (float64-kernel-sin y0 y1 1))))))))
