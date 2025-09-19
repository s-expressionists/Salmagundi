(in-package #:salmagundi-extrinsic/benchmark)

(defclass bucket/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/fnv-hash:client)
  ())

(defclass bucket/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/bucket:bucket-client
     salmagundi/sip-hash:client)
  ())

(defclass block/fnv
    (salmagundi-extrinsic:extrinsic-client salmagundi/block-hash-table:client
     salmagundi/fnv-hash:client)
  ())

(defclass block/sip
    (salmagundi-extrinsic:extrinsic-client salmagundi/block-hash-table:client
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

(defmethod salmagundi:hash-table-count ((hash-table hash-table))
  (hash-table-count hash-table))

(defmethod salmagundi:hash-table-rehash-size ((hash-table hash-table))
  (hash-table-rehash-size hash-table))

(defmethod salmagundi:hash-table-rehash-threshold ((hash-table hash-table))
  (hash-table-rehash-threshold hash-table))

(defmethod salmagundi:hash-table-size ((hash-table hash-table))
  (hash-table-size hash-table))

(defmethod salmagundi:hash-table-test ((hash-table hash-table))
  (hash-table-test hash-table))

(defmethod salmagundi:gethash (key (hash-table hash-table) &optional default)
  (gethash key hash-table default))

(defmethod (setf salmagundi:gethash) (new-value key (hash-table hash-table) &optional default)
  (declare (ignore default))
  (setf (gethash key hash-table) new-value))

(defmethod salmagundi:remhash (key (hash-table hash-table))
  (remhash key hash-table))

(defmethod salmagundi:maphash (function (hash-table hash-table))
  (maphash function hash-table))

(defmethod salmagundi:clrhash ((hash-table hash-table))
  (clrhash hash-table))

(defmethod salmagundi:hash-table-p ((hash-table hash-table))
  t)

(defclass native () ())

(defmethod salmagundi:make-hash-table ((client native) &rest initargs &key)
  (apply #'make-hash-table initargs))

(defvar classes '(native block/fnv bucket/fnv chained/fnv linear-probing/fnv))

(defvar *benchmarks* (make-hash-table))

(defmacro define-benchmark (name &body body)
  `(setf (gethash ',name *benchmarks*)
         (lambda () ,@body)))

(defvar *minimum-bench-time* 10)

(defvar *overhead-time* 0)

(defun bench (thunk)
  (loop with start = (get-internal-real-time)
        with end = (+ start (* *minimum-bench-time* internal-time-units-per-second))
        for count from 1
        finally (return (- (/ (coerce (- (get-internal-real-time) start) 'double-float)
                              internal-time-units-per-second count)
                            *overhead-time*))
        while (< (get-internal-real-time) end)
        do (funcall thunk)))

(defun write-results (results)
  (loop for (name . clients) in results
        do (write-line
            (cl-spark:vspark clients
                             :title name
                             :min 0
                             :size 132
                             :key #'cdr
                             :labels (mapcar #'car clients)))))

(defun run (&rest names)
  (loop with *overhead-time* = 0
        with clients = (mapcar #'make-instance classes)
        for name being each hash-key in *benchmarks* using (hash-value thunk)
        initially (format t "Measuring overhead...~%")
                  (setf *overhead-time* (bench (lambda () ())))
        finally (write-results results)
        when (or (null names)
                 (find name names :test (lambda (x y)
                                          (equalp (symbol-name x) (symbol-name y)))))
          do (format t "Running ~a...~%" name)
        collect (cons name
                      (loop for class in classes
                            for salmagundi-extrinsic:*client* in clients
                            do (format t "  ~a...~%" class)
                            collect (cons class (bench thunk))))
        into results))
