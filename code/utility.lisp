(in-package #:salmagundi)

(declaim (ftype (function (fixnum) fixnum) ceiling-pow2)
         (ftype (function (fixnum fixnum) fixnum) mod-pow2)
         (inline ceiling-pow2 mod-pow2))

(defun ceiling-pow2 (x)
  (ash 1 (integer-length (1- x))))

(defun mod-pow2 (x y)
  (logand x (1- y)))
