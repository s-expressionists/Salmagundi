(cl:in-package #:salmagundi)

(defconstant +character-seed+ 2)
(defconstant +integer-seed+ 3)
(defconstant +float-seed+ 3)
(defconstant +ratio-seed+ 4)
(defconstant +complex-seed+ 5)
(defconstant +cons-seed+ 5)

(defmethod eq-hash ((client standard-client) value &optional hash)
  (hash client
        #+abcl (system:identity-hash-code value)
        #+allegro (excl:lispval-to-address value)
        #+ccl (ccl:%address-of value)
        #+cmucl (lisp::get-lisp-obj-address value)
        #+ecl (si:pointer value)
        #+sbcl (sb-kernel:get-lisp-obj-address value)
        hash))

(defmethod eql-hash ((client standard-client) value &optional hash)
  (eq-hash client value hash))

(defmethod eql-hash ((client standard-client) (value character) &optional hash)
  (hash client (char-code value) (hash client +character-seed+ hash)))

(defmethod eql-hash ((client standard-client) (value integer) &optional hash)
  (hash client value (hash client +integer-seed+ hash)))

(defmethod eql-hash ((client standard-client) (value float) &optional hash)
  (setf hash (hash client +float-seed+ hash))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 value)
    (if (symbolp exponent)
        ; The payload of NaN is ignored in comparisons and infinity doesn't have a payload.
        (hash client sign (eq-hash client exponent hash))
        (hash client sign (hash client exponent (hash client significand hash))))))

(defvar *depth* 3)

(defmethod eql-hash ((client standard-client) (value ratio) &optional hash)
  (hash client (denominator value) (hash client (numerator value) (hash client +ratio-seed+ hash))))

(defmethod eql-hash ((client standard-client) (value complex) &optional hash)
  (hash client (imagpart value) (hash client (realpart value) (hash client +complex-seed+ hash))))

(defmethod equal-hash :around ((client standard-client) value &optional hash)
  (let ((*depth* (1- *depth*)))
    (if (zerop *depth*)
        hash
        (call-next-method))))

(defmethod equal-hash ((client standard-client) value &optional hash)
  (eql-hash client value hash))

(defmethod equal-hash ((client standard-client) (value cons) &optional hash)
  (equal-hash client (cdr value) (equal-hash client (car value) (hash client +complex-seed+ hash))))

(defmethod equal-hash ((client standard-client) (value string) &optional hash)
  (loop for ch across value
        repeat 16
        do (setf hash (eql-hash client ch hash)))
  hash)

(defmethod equal-hash ((client standard-client) (value bit-vector) &optional hash)
  (loop with integer = 0
        for bit across value
        for index below (integer-length most-positive-fixnum)
        finally (return (hash client integer hash))
        do (setf (ldb (byte 1 index) integer) bit)))

(defmethod equalp-hash ((client standard-client) value &optional hash)
  (equal-hash client value hash))

(defmethod equalp-hash ((client standard-client) (value character) &optional hash)
  (hash client (char-code (char-downcase value)) (hash client +character-seed+ hash)))

(defmethod equalp-hash ((client standard-client) (value real) &optional hash)
  (setf hash (hash client +float-seed+ hash))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 (coerce value 'long-float))
    (cond ((symbolp exponent)
           ; The payload of NaN is ignored in comparisons and infinity doesn't have a payload.
           (hash client sign (eq-hash client exponent hash)))
          ((and (zerop exponent) (zerop significand))
           ; Ignore the sign for zero
           (hash client 0 hash))
          (t
           (hash client sign (hash client exponent (hash client significand hash)))))))

(defmethod equalp-hash ((client standard-client) (value complex) &optional hash)
  (setf hash (equalp-hash client (realpart value) hash))
  (unless (zerop (imagpart value))
    (setf hash (equalp-hash client (imagpart value) hash)))
  hash)

