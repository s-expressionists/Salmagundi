(cl:in-package #:salmagundi/chained-hash-table)

(defclass client (salmagundi:standard-client) ())

(defstruct entry
  (hash 0 :read-only t :type fixnum)
  (key nil :read-only t)
  (value nil))

(defclass chained-hash-table (salmagundi:hash-table)
  ((data :accessor hash-table-data)
   (count :accessor %hash-table-count
          :reader salmagundi:hash-table-count
          :initform 0))
  (:default-initargs :rehash-threshold 1.5))

(defmethod initialize-instance :after ((hash-table chained-hash-table) &key (size 16))
  (setf (hash-table-data hash-table)
        (make-array (salmagundi:ceiling-pow2 size)
                    :initial-element nil :element-type 'list)))

(defmethod salmagundi:make-hash-table ((client client) &rest initargs
                                       &key test size rehash-size rehash-threshold
                                            hash-function)
  (declare (ignore test size rehash-size rehash-threshold))
  (apply #'make-instance 'chained-hash-table initargs))

(defmethod salmagundi:hash-table-size ((hash-table chained-hash-table))
  (length (hash-table-data hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table chained-hash-table))
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
          (values t (entry-key entry) (entry-value entry)))))))

(defun compute-rehash-size (hash-table)
  (let ((rehash-size (salmagundi:hash-table-rehash-size hash-table))
        (size (salmagundi:hash-table-size hash-table)))
    (+ size
       (if (integerp rehash-size)
           rehash-size
           (ceiling (* size rehash-size))))))

(defmethod salmagundi:rehash
    ((hash-table chained-hash-table) &key (size (compute-rehash-size hash-table)))
  (setf size (salmagundi:ceiling-pow2 size))
  (with-accessors ((data hash-table-data))
      hash-table
    (loop with new-data = (make-array size :initial-element nil :element-type 'list)
          for slot across data
          finally (setf data new-data)
          do (loop for entry in slot
                   do (push entry (aref new-data (salmagundi:mod-pow2 (entry-hash entry) size)))))))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (salmagundi:hash-table-count hash-table)
           (* (salmagundi:hash-table-size hash-table)
              (salmagundi:hash-table-rehash-threshold hash-table)))
    (salmagundi:rehash hash-table)))

(defun find-entry (hash-table key)
  (loop with test of-type (or symbol function) = (salmagundi:hash-table-test hash-table)
        with hash = (funcall (the (or symbol function)
                                  (salmagundi:hash-table-hash-function hash-table))
                             key)
        with index = (salmagundi:mod-pow2 hash (salmagundi:hash-table-size hash-table))
        for previous-head = nil then head
        for head on (aref (hash-table-data hash-table) index)
        for entry = (car head)
        finally (return (values nil hash index nil))
        when (and (= hash (entry-hash entry))
                  (funcall test key (entry-key entry)))
          return (values entry hash index previous-head)))

(defmethod (setf salmagundi:gethash)
    (value key (hash-table chained-hash-table) &optional default)
  (declare (ignore default))
  (multiple-value-bind (entry hash index)
      (find-entry hash-table key)
    (cond (entry
           (setf (entry-value entry) value))
          (t
           (push (make-entry :hash hash :key key :value value)
                 (aref (hash-table-data hash-table) index))
           (incf (%hash-table-count hash-table))
           (maybe-grow-and-rehash hash-table))))
  value)

(defmethod salmagundi:gethash (key (hash-table chained-hash-table) &optional default)
  (let ((entry (find-entry hash-table key)))
    (if entry
        (values (entry-value entry) t)
        (values default nil))))

(defmethod salmagundi:remhash (key (hash-table chained-hash-table))
  (multiple-value-bind (entry hash index previous-head)
      (find-entry hash-table key)
    (declare (ignore hash))
    (when entry
      (if previous-head
          (setf (cdr previous-head) (cddr previous-head))
          (pop (aref (hash-table-data hash-table) index)))
      (decf (%hash-table-count hash-table))
      t)))

(defmethod salmagundi:clrhash ((hash-table chained-hash-table))
  (fill (hash-table-data hash-table) nil)
  (setf (%hash-table-count hash-table) 0)
  hash-table)

(defmethod salmagundi:maphash (function (hash-table chained-hash-table))
  (loop for bucket across (hash-table-data hash-table)
        do (loop for entry in bucket
                 do (funcall function (entry-key entry) (entry-value entry)))))
