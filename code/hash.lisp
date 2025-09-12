(cl:in-package #:salmagundi)

(defun slot-names (class)
  #+abcl
  (mapcar (lambda (slot)
            (system::dsd-name slot))
          (mop:class-slots class))
  #+ccl
  (mapcar #'ccl:slot-definition-name
          (ccl:class-slots class))
  #+(or clasp clisp ecl lispworks scl)
  (mapcar #'clos:slot-definition-name
          (clos:class-slots class))
  #+cmucl
  (mapcar #'clos-mop:slot-definition-name
          (clos-mop:class-slots class))
  #+mezzano
  (mapcar #'mezzano.clos:slot-definition-name
          (mezzano.clos:class-slots class))
  #+sicl
  (mapcar #'sicl-clos:slot-definition-name
          (sicl-clos:class-slots class))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots class)))

(defconstant +object-seed+ 1)
(defconstant +character-seed+ 2)
(defconstant +integer-seed+ 3)
(defconstant +float-seed+ 3)
(defconstant +ratio-seed+ 4)
(defconstant +complex-seed+ 5)
(defconstant +cons-seed+ 5)
(defconstant +array-seed+ 6)
(defconstant +symbol-seed+ 7)
(defconstant +package-seed+ 8)
(defconstant +string-seed+ 9)
(defconstant +bit-vector-seed+ 10)
(defconstant +pathname-seed+ 11)
(defconstant +hash-table-seed+ 12)
(defconstant +structure-seed+ 13)

(defvar *hash-limit* 10)

(defmacro hash* (client state &rest values)
  (reduce (lambda (state value)
            `(hash ,client ,value ,state))
          values
          :initial-value state))

(defun hash-string (client value state &optional case-insensitive-p)
  (loop for ch across value
        repeat *hash-limit*
          initially (setf state (hash* client state +string-seed+ (length value)))
        do (setf state (hash client
                             (char-code (if case-insensitive-p
                                            (char-downcase ch)
                                            ch))
                             state)))
  state)

(defmethod equivalence-hash :around ((client standard-client) equivalence value &optional state)
  (let ((*hash-limit* (1- *hash-limit*)))
    (if (zerop *hash-limit*)
        state
        (call-next-method))))

(defclass eq-hash () ())

(defmethod equivalence-hash
    ((client standard-client) (equivalence eq-hash) value &optional state)
  (hash* client
         state
         +object-seed+
         #+abcl (system:identity-hash-code value)
         #+allegro (excl:lispval-to-address value)
         #+ccl (ccl:%address-of value)
         #+cmucl (lisp::get-lisp-obj-address value)
         #+ecl (si:pointer value)
         #+sbcl (sb-kernel:get-lisp-obj-address value)))

(defclass eql-hash (eq-hash) ())

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value character) &optional state)
  (hash* client state +character-seed+ (char-code value)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence eql-hash) (value integer) &optional state)
  (hash* client state +integer-seed+ value))

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
                    (equivalence-hash client equivalence (realpart value)
                                      (hash client +complex-seed+ state))))

(defclass equal-hash (eql-hash) ())

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value cons) &optional state)
  (equivalence-hash client equivalence (cdr value)
                    (equivalence-hash client equivalence (car value)
                                      (hash client +complex-seed+ state))))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value string) &optional state)
  (hash-string client value (hash client +string-seed+ state)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value bit-vector) &optional state)
  (loop with integer = 0
        for bit across value
        for index below (integer-length most-positive-fixnum)
        initially (setf state (hash client +bit-vector-seed+ state))
        finally (return (hash client integer state))
        do (setf (ldb (byte 1 index) integer) bit)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence equal-hash) (value pathname) &optional state)
  (setf state (hash client +pathname-seed+ state))
  (setf state (equivalence-hash client equivalence (pathname-host value) state))
  (setf state (equivalence-hash client equivalence (pathname-device value) state))
  (setf state (equivalence-hash client equivalence (pathname-directory value) state))
  (setf state (equivalence-hash client equivalence (pathname-name value) state))
  (setf state (equivalence-hash client equivalence (pathname-type value) state))
  (setf state (equivalence-hash client equivalence (pathname-version value) state))
  state)

(defclass equalp-hash (equal-hash) ())

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
    ((client standard-client) (equivalence equalp-hash) (value array) &optional state)
  (setf state (hash* client state +array-seed+ (array-rank value)))
  (loop for dim in (array-dimensions value)
        do (setf state (hash client dim state)))
  (loop for i below (array-total-size value)
        repeat *hash-limit*
        do (setf state (equivalence-hash client equivalence (row-major-aref value i) state)))
  state)

(defmethod equivalence-hash
    ((client standard-client) (equivalence equalp-hash) (table hash-table) &optional state)
  (setf state (hash* client state
                     +hash-table-seed+
                     (hash-table-count table)))
  (setf state (eqivalence-hash client equivalence (hash-table-test table) state))
  (let ((hash-function (hash-table-hash-function table))
        (limit *hash-limit*))
    (maphash (lambda (key value)
               (cond ((plusp limit)
                      (setf state (equivalence-hash client equivalence value (hash client (funcall hash-function key) state)))
                      (decf limit))
                     (t
                      (return-from equivalence-hash state))))
             table))
  state)

(defmethod equivalence-hash
    ((client standard-client) (equivalence equalp-hash) (object structure-object) &optional state)
  (let ((class (class-of object)))
    (setf state (equivalence-hash client equivalence (class-name class)
                                  (hash client +structure-seed+ state)))
    (loop for name in (class-slot-names client class)
          repeat *hash-limit*
          do (setf state (equivalence-hash client equivalence name state))
          when (slot-boundp object name)
            do (setf state (equivalence-hash client equivalence (slot-value object name) state)))
    state))

(defclass similarp-hash (equal-hash) ())

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
  (hash-string client (symbol-name value)
               (hash client +symbol-seed+ state)))

(defmethod equivalence-hash
    ((client standard-client) (equivalence similarp-hash) (value package) &optional state)
  (hash-string client (package-name value)
               (hash client +package-seed+ state)))
