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
(defconstant +short-float-seed+ 3)
(defconstant +single-float-seed+ 4)
(defconstant +double-float-seed+ 5)
(defconstant +long-float-seed+ 6)
(defconstant +ratio-seed+ 7)
(defconstant +complex-seed+ 8)
(defconstant +cons-seed+ 9)
(defconstant +array-seed+ 10)
(defconstant +symbol-seed+ 11)
(defconstant +package-seed+ 12)
(defconstant +string-seed+ 13)
(defconstant +bit-vector-seed+ 14)
(defconstant +pathname-seed+ 15)
(defconstant +hash-table-seed+ 16)
(defconstant +structure-seed+ 17)

(defvar *hash-limit* 10)

(defun hash-string (client state value &optional case-insensitive-p)
  (loop for ch across value
        repeat *hash-limit*
        initially (hash client state +string-seed+)
                  (hash client state (length value))
        do (hash client state
                 (char-code (if case-insensitive-p
                                (char-downcase ch)
                                ch)))))

(defun hash-float (client state value &optional similarp)
  (hash client state
        (etypecase value
          #+quaviver/short-float
          (short-float +short-float-seed+)
          (single-float +single-float-seed+)
          (double-float +double-float-seed+)
          #+quaviver/long-float
          (long-float +long-float-seed+)))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 2 value)
    (cond ((symbolp exponent)
           ; The payload of NaN is ignored in equivalences and infinity doesn't have a payload.
           (hash client state
                 (ecase exponent
                  (:infinity 1)
                  (:quiet-nan 2)
                  (:signaling-nan 3)))
           (hash client state sign))
          ((and similarp (zerop exponent) (zerop significand))
           ; Ignore the sign for zero
           (hash client state 0))
          (t
           (hash client state significand)
           (hash client state exponent)
           (hash client state sign)))))

(defmethod equivalence-hash :around ((client standard-client) state equivalence value)
  (let ((*hash-limit* (1- *hash-limit*)))
    (when (plusp *hash-limit*)
      (call-next-method))))

(defclass eq-hash () ())

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eq-hash) value)
  (hash client state +object-seed+)
  (hash client state
        #-(or abcl allegro ccl cmucl ecl sbcl) 0
        #+abcl (system:identity-hash-code value)
        #+allegro (excl:lispval-to-address value)
        #+ccl (ccl:%address-of value)
        #+cmucl (lisp::get-lisp-obj-address value)
        #+ecl (si:pointer value)
        #+sbcl (sb-kernel:get-lisp-obj-address value)))

(defclass eql-hash (eq-hash) ())

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eql-hash) (value character))
  (hash client state +character-seed+)
  (hash client state (char-code value)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eql-hash) (value integer))
  (hash client state +integer-seed+)
  (hash client state value))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eql-hash) (value float))
  (hash-float client state equivalence value))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eql-hash) (value ratio))
  (hash client state +ratio-seed+)
  (hash client state (denominator value))
  (hash client state (numerator value)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence eql-hash) (value complex))
  (hash client state +complex-seed+)
  (equivalence-hash client state equivalence (realpart value))
  (equivalence-hash client state equivalence (imagpart value)))

(defclass equal-hash (eql-hash) ())

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equal-hash) (value cons))
  (hash client state +cons-seed+)
  (equivalence-hash client state equivalence (car value))
  (equivalence-hash client state equivalence (cdr value)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equal-hash) (value string))
  (hash client state +string-seed+)
  (hash-string client state value))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equal-hash) (value bit-vector))
  (loop with integer = 0
        for bit across value
        for index below (integer-length most-positive-fixnum)
        repeat *hash-limit*
        initially (hash client state +bit-vector-seed+)
                  (hash client state (length value))
        finally (hash client state integer)
        do (setf (ldb (byte 1 index) integer) bit)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equal-hash) (value pathname))
  (hash client state +pathname-seed+)
  (equivalence-hash client state equivalence (pathname-host value))
  (equivalence-hash client state equivalence (pathname-device value))
  (equivalence-hash client state equivalence (pathname-directory value))
  (equivalence-hash client state equivalence (pathname-name value))
  (equivalence-hash client state equivalence (pathname-type value))
  (equivalence-hash client state equivalence (pathname-version value)))

(defclass equalp-hash (equal-hash) ())

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (value character))
  (hash client state +character-seed+)
  (hash client state (char-code (char-downcase value))))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (value real))
  (hash-float client state equivalence (coerce value 'long-float) t))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (value complex))
  (cond ((zerop (imagpart value))
         (equivalence-hash client state equivalence (realpart value)))
        (t
         (hash client state +complex-seed+)
         (equivalence-hash client state equivalence (realpart value))
         (equivalence-hash client state equivalence (imagpart value)))))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (value array))
  (hash client state +array-seed+)
  (hash client state (array-rank value))
  (loop for dim in (array-dimensions value)
        do (hash client state dim))
  (loop for i below (array-total-size value)
        repeat *hash-limit*
        do (equivalence-hash client state equivalence (row-major-aref value i))))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (table hash-table))
  (hash client state +hash-table-seed+)
  (hash client state (hash-table-count table))
  (eqivalence-hash client state equivalence (hash-table-test table))
  (let ((hash-function (hash-table-hash-function table))
        (limit *hash-limit*))
    (maphash (lambda (key value)
               (cond ((plusp limit)
                      (hash client state (funcall hash-function key))
                      (equivalence-hash client state equivalence value)
                      (decf limit))
                     (t
                      (return-from equivalence-hash state))))
             table)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence equalp-hash) (object structure-object))
  (loop with class = (class-of object)
        for name in (class-slot-names client class)
        repeat *hash-limit*
        initially (hash client state +structure-seed+)
                    (equivalence-hash client state equivalence (class-name class))
        do (equivalence-hash client state equivalence name)
        when (slot-boundp object name)
          do (equivalence-hash client state equivalence (slot-value object name))))

(defclass similarp-hash (equal-hash) ())

(defmethod equivalence-hash
    ((client standard-client) state (equivalence similarp-hash) (value float))
  (hash-float client state equivalence value t))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence similarp-hash) (value symbol))
  (hash client state +symbol-seed+)
  (hash-string client state (symbol-name value)))

(defmethod equivalence-hash
    ((client standard-client) state (equivalence similarp-hash) (value package))
  (hash client state +package-seed+)
  (hash-string client state (package-name value)))
