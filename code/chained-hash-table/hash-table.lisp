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
  (:default-initargs :rehash-threshold 1.0
                     :rehash-size 2.0))

(defmethod initialize-instance :after ((hash-table chained-hash-table) &key (size 16))
  (setf (hash-table-data hash-table)
        (make-array (salmagundi:ceiling-pow2 size)
                    :initial-element nil :element-type 'list)))

(defmethod salmagundi:make-hash-table ((client client) &rest initargs
                                       &key test size rehash-size rehash-threshold
                                            hash-function)
  (declare (ignore test size rehash-size rehash-threshold hash-function))
  (apply #'make-instance 'chained-hash-table initargs))

(defmethod salmagundi:hash-table-size ((hash-table chained-hash-table))
  (length (hash-table-data hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table chained-hash-table))
  (let* ((data (hash-table-data hash-table))
         (index (length data))
         (entries nil)
         (entry nil))
    (declare (type (vector list) data)
             (type fixnum index)
             (type list entries)
             (type (or null entry) entry))
    (lambda ()
      (block hash-table-iterator
        (tagbody
         next-entry
           (when entries
             (setf entry (pop entries))
             (return-from hash-table-iterator (values t (entry-key entry) (entry-value entry))))
         next-chain
           (when (minusp (decf index))
             (return-from hash-table-iterator nil))
           (setf entries (aref data index))
           (go next-entry))))))

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
  (prog* ((data (hash-table-data hash-table))
          (index (length data))
          (new-data (make-array size :initial-element nil :element-type 'list))
          (entries nil)
          (entry nil))
     (declare (type list entries)
              (type (or null entry) entry))
   next-chain
     (when (minusp (decf index))
       (setf (hash-table-data hash-table) new-data)
       (return nil))
     (setf entries (aref data index))
   next-entry
     (unless entries
       (go next-chain))
     (setf entry (pop entries))
     (push entry (aref new-data (salmagundi:mod-pow2 (entry-hash entry) size)))
     (go next-entry)))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (salmagundi:hash-table-count hash-table)
           (* (salmagundi:hash-table-size hash-table)
              (salmagundi:hash-table-rehash-threshold hash-table)))
    (salmagundi:rehash hash-table)))

(defun find-entry (hash-table key)
  (prog* ((test (salmagundi:hash-table-test hash-table))
          (hash (funcall (the (or symbol function)
                          (salmagundi:hash-table-hash-function hash-table))
                 key))
          (index (salmagundi:mod-pow2 hash (salmagundi:hash-table-size hash-table)))
          (previous-cons nil)
          (entries (aref (hash-table-data hash-table) index))
          (entry nil))
     (declare (type (or symbol function) test)
              (type fixnum hash index)
              (type (or null entry) entry)
              (type list entries previous-cons))
   next-entry
     (unless entries
       (return (values nil hash index nil)))
     (setf entry (car entries))
     (when (and (= hash (entry-hash entry))
                (funcall test key (entry-key entry)))
       (return (values entry hash index previous-cons)))
     (setf previous-cons entries
           entries (cdr entries))
     (go next-entry)))

(defmethod (setf salmagundi:gethash)
    (value key (hash-table chained-hash-table) &optional default)
  (declare (ignore default))
  (multiple-value-bind (entry hash index)
      (find-entry hash-table key)
    (declare (type (or null entry) entry)
             (type fixnum hash index))
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
    (declare (type (or null entry) entry))
    (if entry
        (values (entry-value entry) t)
        (values default nil))))

(defmethod salmagundi:remhash (key (hash-table chained-hash-table))
  (multiple-value-bind (entry hash index previous-cons)
      (find-entry hash-table key)
    (declare (ignore hash)
             (type (or null entry) entry)
             (type fixnum hash index)
             (type list previous-cons))
    (when entry
      (if previous-cons
          (setf (cdr previous-cons) (cddr previous-cons))
          (pop (aref (hash-table-data hash-table) index)))
      (decf (%hash-table-count hash-table))
      t)))

(defmethod salmagundi:clrhash ((hash-table chained-hash-table))
  (fill (hash-table-data hash-table) nil)
  (setf (%hash-table-count hash-table) 0)
  hash-table)

(defmethod salmagundi:maphash (function (hash-table chained-hash-table))
  (prog* ((data (hash-table-data hash-table))
          (index (length data))
          (entries nil)
          (entry nil))
  (declare (type fixnum index)
           (type list entries)
           (type (or null entry) entry))
   next-chain
     (when (minusp (decf index))
       (return nil))
     (setf entries (aref data index))
   next-entry
     (unless entries
       (go next-chain))
     (setf entry (pop entries))
     (funcall function (entry-key entry) (entry-value entry))
     (go next-entry)))
