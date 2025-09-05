(cl:in-package #:salmagundi/bucket)

(defclass bucket-client (salmagundi:standard-client) ())

(defclass bucket-hash-table (salmagundi:hash-table)
  ((size :accessor %bucket-hash-table-size
         :reader salmagundi:hash-table-size
         :initarg :size
         :initform 16)
   (data :accessor hash-table-data)
   (count :accessor %bucket-hash-table-count
          :reader salmagundi:hash-table-count
          :initform 0)))

(defmethod initialize-instance :after ((hash-table bucket-hash-table) &key)
  (setf (hash-table-data hash-table)
        (make-array (salmagundi:hash-table-size hash-table)
                    :initial-element '())))

(defmethod salmagundi:make-hash-table ((client bucket-client) &rest initargs
                                       &key test size rehash-size rehash-threshold)
  (declare (ignore test size rehash-size rehash-threshold))
  (apply #'make-instance 'bucket-hash-table initargs))

(defmethod salmagundi:make-hash-table-iterator ((hash-table bucket-hash-table))
  (let ((data (hash-table-data hash-table))
        (size (salmagundi:hash-table-size hash-table))
        (position 0)
        (contents '()))
    (lambda ()
      (block hash-table-iterator
        ;; Look for the next bucket which contains mappings.
        (loop while (null contents)
              when (= position size)
                ;; When we run out of mappings, we return the single value NIL.
                do (return-from hash-table-iterator nil)
              do (setf contents (aref data position))
                 (incf position))
        ;; Otherwise, we return values T, key and value.
        (let ((entry (pop contents)))
          (values t (car entry) (cdr entry)))))))

(defun grow-and-rehash (hash-table)
  (with-accessors ((rehash-size salmagundi:hash-table-rehash-size)
                   (size salmagundi:hash-table-size)
                   (hash-function salmagundi:hash-table-hash-function))
      hash-table
    (let* ((new-size (if (integerp rehash-size)
                         (+ size rehash-size)
                         (round (* size rehash-size))))
           (new-data (make-array new-size :initial-element '())))
      (salmagundi:maphash (lambda (key value)
                            (let* ((key-hash (funcall hash-function key))
                                   (index (mod key-hash new-size)))
                              (push (cons key value) (aref new-data index))))
                          hash-table)
      (setf (hash-table-data hash-table) new-data
            (%bucket-hash-table-size hash-table) new-size))))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (salmagundi:hash-table-count hash-table)
           (* (salmagundi:hash-table-size hash-table)
              (salmagundi:hash-table-rehash-threshold hash-table)))
    (grow-and-rehash hash-table)))

(defmacro with-entry ((entries-index entries entry key hash-table) &body body)
  (let ((key-hash (gensym)))
    `(let* ((,key-hash (funcall (salmagundi:hash-table-hash-function ,hash-table)
                                ,key))
            (,entries-index (mod ,key-hash (salmagundi:hash-table-size ,hash-table)))
            (,entries (aref (hash-table-data ,hash-table) index))
            (,entry (assoc ,key entries :test (salmagundi:hash-table-test ,hash-table))))
       ,@body)))

(defmethod (setf salmagundi:gethash) (value key (hash-table bucket-hash-table) &optional default)
  (declare (ignore default))
  (with-entry (index entries entry key hash-table)
    (cond (entry
           (setf (cdr entry) value))
          (t
           (push (cons key value) (aref (hash-table-data hash-table) index))
           (incf (%bucket-hash-table-count hash-table))
           (maybe-grow-and-rehash hash-table))))
  value)

(defmethod salmagundi:gethash (key (hash-table bucket-hash-table) &optional default)
  (with-entry (index entries entry key hash-table)
    (if (null entry)
        (values default nil)
        (values (cdr entry) t))))

(defmethod salmagundi:remhash (key (hash-table bucket-hash-table))
  (with-entry (index entries entry key hash-table)
    (unless (null entry)
      (setf (aref (hash-table-data hash-table) index)
            (delete key entries :key #'car :test (salmagundi:hash-table-test hash-table)))
      (decf (%bucket-hash-table-count hash-table)))
    (not (null entry))))

(defmethod salmagundi:clrhash ((hash-table bucket-hash-table))
  (fill (hash-table-data hash-table) '())
  (setf (%bucket-hash-table-count hash-table) 0)
  hash-table)

(defmethod salmagundi:maphash (function (hash-table bucket-hash-table))
  (loop for bucket across (hash-table-data hash-table)
        do (loop for (key . value) in bucket
                 do (funcall function key value))))
