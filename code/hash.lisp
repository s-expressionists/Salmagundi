(cl:in-package #:salmagundi)

(defconstant +character-seed+ 2)
(defconstant +integer-seed+ 3)
(defconstant +float-seed+ 3)
(defconstant +ratio-seed+ 4)
(defconstant +complex-seed+ 5)
(defconstant +cons-seed+ 5)
(defconstant +array-seed+ 6)
(defconstant +symbol-seed+ 7)

(defvar *hash-limit* 10)

(defmethod equivalence-hash :around ((client standard-client) equivalence value &optional state)
  (let ((*hash-limit* (1- *hash-limit*)))
    (if (zerop *hash-limit*)
        state
        (call-next-method))))

(defclass eq-hash () ())

(defclass eql-hash (eq-hash) ())

(defclass equal-hash (eql-hash) ())

(defclass equalp-hash (equal-hash) ())

(defclass similarp-hash (equal-hash) ())

(defmethod equivalence-hash
    ((client standard-client) (equivalence eq-hash) value &optional state)
  (hash client
        #+abcl (system:identity-hash-code value)
        #+allegro (excl:lispval-to-address value)
        #+ccl (ccl:%address-of value)
        #+cmucl (lisp::get-lisp-obj-address value)
        #+ecl (si:pointer value)
        #+sbcl (sb-kernel:get-lisp-obj-address value)
        state))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value character) &optional state)
  (hash client (char-code value) (hash client +character-seed+ state)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value integer) &optional state)
  (hash client value (hash client +integer-seed+ state)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value float) &optional state)
  (setf state (hash client +float-seed+ state))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 value)
    (if (symbolp exponent)
        ; The payload of NaN is ignored in equivalences and infinity doesn't have a payload.
        (hash client sign (hash client exponent state))
        (hash client sign (hash client exponent (hash client significand state))))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value ratio) &optional state)
  (hash client (denominator value)
        (hash client (numerator value)
              (hash client +ratio-seed+ state))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value complex) &optional state)
  (equivalence-hash client equivalence (imagpart value)
                    (equivalence-hash client equivalence(realpart value)
                                      (hash client +complex-seed+ state))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value cons) &optional state)
  (equivalence-hash client equivalence (cdr value)
                    (equivalence-hash client equivalence (car value)
                                      (hash client +complex-seed+ state))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value string) &optional state)
  (setf state (hash client +array-seed+ state))
  (loop for ch across value
        repeat *hash-limit*
        do (setf state (equivalence-hash client equivalence ch state)))
  state)

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value bit-vector) &optional state)
  (loop with integer = 0
        for bit across value
        for index below (integer-length most-positive-fixnum)
        finally (return (hash client integer state))
        do (setf (ldb (byte 1 index) integer) bit)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equalp-hash) (value character) &optional state)
  (hash client (char-code (char-downcase value)) (hash client +character-seed+ state)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equalp-hash) (value real) &optional state)
  (setf state (hash client +float-seed+ state))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 (coerce value 'long-float))
    (cond ((symbolp exponent)
           ; The payload of NaN is ignored in equivalences and infinity doesn't have a payload.
           (hash client sign (hash client exponent state)))
          ((and (zerop exponent) (zerop significand))
           ; Ignore the sign for zero
           (hash client 0 state))
          (t
           (hash client sign (hash client exponent (hash client significand state)))))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equalp-hash) (value complex) &optional state)
  (setf state (equivalence-hash client equivalence (realpart value) state))
  (unless (zerop (imagpart value))
    (setf state (equivalence-hash client equivalence (imagpart value) state)))
  state)

(defmethod equivalence-hash
    ((client standard-client) (equivalence similarp-hash) (value float) &optional state)
  (setf state (hash client +float-seed+ state))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 value)
    (cond ((symbolp exponent)
           ; The payload of NaN is ignored in equivalences and infinity doesn't have a payload.
           (hash client sign (hash client exponent state)))
          ((and (zerop exponent) (zerop significand))
           ; Ignore the sign for zero
           (hash client 0 state))
          (t
           (hash client sign (hash client exponent (hash client significand state)))))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence similarp-hash) (value symbol) &optional state)
  (equivalence-hash client equivalence (symbol-name value)
                    (hash client +symbol-seed+ state)))
