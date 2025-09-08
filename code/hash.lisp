(cl:in-package #:salmagundi)

(defconstant +character-seed+ 2)
(defconstant +integer-seed+ 3)
(defconstant +float-seed+ 3)
(defconstant +ratio-seed+ 4)
(defconstant +complex-seed+ 5)
(defconstant +cons-seed+ 5)

(defmethod eq-hash ((client standard-client) value &optional state)
  (hash client
        #+abcl (system:identity-hash-code value)
        #+allegro (excl:lispval-to-address value)
        #+ccl (ccl:%address-of value)
        #+cmucl (lisp::get-lisp-obj-address value)
        #+ecl (si:pointer value)
        #+sbcl (sb-kernel:get-lisp-obj-address value)
        state))

(defmethod eql-hash ((client standard-client) value &optional state)
  (eq-hash client value state))

(defmethod eql-hash ((client standard-client) (value character) &optional state)
  (hash client (char-code value) (hash client +character-seed+ state)))

(defmethod eql-hash ((client standard-client) (value integer) &optional state)
  (hash client value (hash client +integer-seed+ state)))

(defmethod eql-hash ((client standard-client) (value float) &optional state)
  (setf state (hash client +float-seed+ state))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 value)
    (if (symbolp exponent)
        ; The payload of NaN is ignored in comparisons and infinity doesn't have a payload.
        (hash client sign (eq-hash client exponent state))
        (hash client sign (hash client exponent (hash client significand state))))))

(defvar *depth* 3)

(defmethod eql-hash ((client standard-client) (value ratio) &optional state)
  (hash client (denominator value) (hash client (numerator value) (hash client +ratio-seed+ state))))

(defmethod eql-hash ((client standard-client) (value complex) &optional state)
  (hash client (imagpart value) (hash client (realpart value) (hash client +complex-seed+ state))))

(defmethod equal-hash :around ((client standard-client) value &optional state)
  (let ((*depth* (1- *depth*)))
    (if (zerop *depth*)
        hash
        (call-next-method))))

(defmethod equal-hash ((client standard-client) value &optional state)
  (eql-hash client value state))

(defmethod equal-hash ((client standard-client) (value cons) &optional state)
  (equal-hash client (cdr value) (equal-hash client (car value) (hash client +complex-seed+ state))))

(defmethod equal-hash ((client standard-client) (value string) &optional state)
  (loop for ch across value
        repeat 16
        do (setf state (eql-hash client ch state)))
  state)

(defmethod equal-hash ((client standard-client) (value bit-vector) &optional state)
  (loop with integer = 0
        for bit across value
        for index below (integer-length most-positive-fixnum)
        finally (return (hash client integer state))
        do (setf (ldb (byte 1 index) integer) bit)))

(defmethod equalp-hash ((client standard-client) value &optional state)
  (equal-hash client value state))

(defmethod equalp-hash ((client standard-client) (value character) &optional state)
  (hash client (char-code (char-downcase value)) (hash client +character-seed+ state)))

(defmethod equalp-hash ((client standard-client) (value real) &optional state)
  (setf state (hash client +float-seed+ state))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 (coerce value 'long-float))
    (cond ((symbolp exponent)
           ; The payload of NaN is ignored in comparisons and infinity doesn't have a payload.
           (hash client sign (eq-hash client exponent state)))
          ((and (zerop exponent) (zerop significand))
           ; Ignore the sign for zero
           (hash client 0 state))
          (t
           (hash client sign (hash client exponent (hash client significand state)))))))

(defmethod equalp-hash ((client standard-client) (value complex) &optional state)
  (setf state (equalp-hash client (realpart value) state))
  (unless (zerop (imagpart value))
    (setf state (equalp-hash client (imagpart value) state)))
  state)
