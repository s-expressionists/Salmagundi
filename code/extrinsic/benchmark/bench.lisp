(in-package #:salmagundi-extrinsic/benchmark)

(defclass bucket/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/fnv-hash:client)
  ())

(defclass bucket/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/sip-hash:client)
  ())

(defclass chained/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/chained-hash-table:client
     salmagundi/fnv-hash:client)
  ())

(defclass chained/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/chained-hash-table:client
     salmagundi/sip-hash:client)
  ())

(defclass linear-probing/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/linear-probing:linear-probing-client
     salmagundi/fnv-hash:client)
  ())

(defclass linear-probing/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/linear-probing:linear-probing-client
     salmagundi/sip-hash:client)
  ())

(defun hash-integers ()
  (the-cost-of-nothing:benchmark
   (loop with table = (salmagundi-extrinsic:make-hash-table)
         for i below 100000
         finally (salmagundi-extrinsic:maphash
                  (lambda (key value)
                    (incf (salmagundi-extrinsic:gethash key table) value))
                  table)
         do (setf (salmagundi-extrinsic:gethash i table) i))))

(defvar classes '(bucket/fnv chained/fnv linear-probing/fnv))

(defun bench ()
  (write-line
   (cl-spark:vspark
    (loop for class in classes
          for salmagundi-extrinsic:*client* = (make-instance class)
          do (format t "~&Running benchmarks on ~a...~%" class)
          collect (hash-integers))
    :min 0
    :size 132
    :labels classes)))
