(cl:in-package #:salmagundi/block-hash-table)

(defclass client (salmagundi:standard-client) ())

(defstruct entry
  (hash 0 :read-only t :type fixnum)
  (key nil :read-only t)
  (value nil))

(deftype element ()
  `(or null fixnum entry))

(defclass block-hash-table (salmagundi:hash-table)
  ((data :accessor hash-table-data)
   (count :accessor %hash-table-count
          :reader salmagundi:hash-table-count
          :initform 0)
   (block-size :accessor block-size
               :initarg :block-size
               :initform 8)
   (bytespec :accessor bytespec))
  (:default-initargs :rehash-threshold 1.0
                     :rehash-size 2.0))

(defmethod initialize-instance :after ((hash-table block-hash-table) &key (size 16))
  (with-accessors ((block-size block-size)
                   (bytespec bytespec)
                   (data hash-table-data))
      hash-table
    (setf block-size (salmagundi:ceiling-pow2 block-size)
          size (* block-size (ceiling size block-size))
          bytespec (byte (- (integer-length size) 2) 0)
          data (make-array size :adjustable t :fill-pointer (ash size -1) :initial-element nil
                                :element-type 'element))))

(defmethod salmagundi:make-hash-table ((client client) &rest initargs
                                       &key test size rehash-size rehash-threshold
                                            hash-function)
  (declare (ignore test size rehash-size rehash-threshold hash-function))
  (apply #'make-instance 'block-hash-table initargs))

(defmethod salmagundi:hash-table-size ((hash-table block-hash-table))
  (array-total-size (hash-table-data hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table block-hash-table))
  (let* ((data (hash-table-data hash-table))
         (index (length data))
         (element nil))
    (declare (type (vector element) data)
             (type fixnum index)
             (type element element))
    (lambda ()
      (block hash-table-iterator
        (tagbody
         next-element
           (when (minusp (decf index))
             (return-from hash-table-iterator nil))
           (when (entry-p (setf element (aref data index)))
             (return-from hash-table-iterator
               (values t (entry-key element) (entry-value element))))
           (go next-element))))))

(defun compute-rehash-size (hash-table &optional (size (salmagundi:hash-table-size hash-table)))
  (let ((rehash-size (salmagundi:hash-table-rehash-size hash-table)))
    (+ size
       (if (integerp rehash-size)
           rehash-size
           (ceiling (* size rehash-size))))))

(defmethod salmagundi:rehash
    ((hash-table block-hash-table) &key (size (compute-rehash-size hash-table)))
  (setf size (salmagundi:ceiling-pow2 size))
  (prog* ((data (hash-table-data hash-table))
          (index (length data))
          (new-index 0)
          (remaining 0)
          (bytespec (byte (- (integer-length size) 2) 0))
          (new-data (make-array size :adjustable t :fill-pointer (ash size -1)
                                     :initial-element nil :element-type 'element))
          (block-size (block-size hash-table))
          (element nil)
          (new-element nil))
     (declare (type element element))
   next-element
     (when (minusp (decf index))
       (setf (hash-table-data hash-table) new-data
             (bytespec hash-table) bytespec)
       (return nil))
     (setf element (aref data index))
     (unless (entry-p element)
       (go next-element))
     (setf new-index (ldb bytespec (entry-hash element))
           new-element (aref new-data new-index))
     (when (integerp new-element)
       (go next-block))
     (unless new-element
       (setf (aref new-data new-index) element)
       (go next-element))
     (go extend-block)
   next-block
     (setf remaining block-size)
   next-new-element
     (setf new-element (aref new-data new-index))
     (when (integerp new-element)
       (setf new-index new-element)
       (go next-block))
     (unless new-element
       (setf (aref new-data new-index) element)
       (go next-element))
     (unless (zerop (decf remaining))
       (incf new-index)
       (go next-new-element))
   extend-block
     (setf (aref new-data new-index) (fill-pointer new-data)
           new-index (fill-pointer new-data))
     (unless (<= (+ (length new-data) block-size) (array-total-size new-data))
       (adjust-array new-data (compute-rehash-size hash-table (array-total-size new-data)) :initial-element nil))
     (incf (fill-pointer new-data) block-size)
     (setf (aref new-data new-index) new-element)
     (incf new-index)
     (setf (aref new-data new-index) element)
     (go next-element)))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (salmagundi:hash-table-count hash-table)
           (* (salmagundi:hash-table-size hash-table)
              (salmagundi:hash-table-rehash-threshold hash-table)))
    (salmagundi:rehash hash-table)))

(defun find-entry (hash-table key &optional makep)
  (prog* ((test (salmagundi:hash-table-test hash-table))
          (hash (funcall (the (or symbol function)
                          (salmagundi:hash-table-hash-function hash-table))
                 key))
          (index (ldb (bytespec hash-table) hash))
          (data (hash-table-data hash-table))
          (element (aref data index))
          (block-size (block-size hash-table))
          (nil-index nil)
          (remaining 0))
     (declare (type (or symbol function) test)
              (type fixnum hash index remaining)
              (type element element)
              (type (or null fixnum) nil-index))
     (when (integerp element)
       (setf index element)
       (go next-block))
     (when (and (null element) makep)
       (incf (%hash-table-count hash-table))
       (return (values (setf (aref data index) (make-entry :hash hash :key key))
                       index)))
     (unless element
       (return nil))
     (when (and (= hash (entry-hash element))
                (funcall test key (entry-key element)))
       (return (values element index)))
     (unless makep
       (return (values nil nil)))
     (go extend-block)
   next-block
     (setf remaining block-size)
   next-element
     (setf element (aref data index))
     (when (integerp element)
       (setf index element)
       (go next-block))
     (when (and element
                (= hash (entry-hash element))
                (funcall test key (entry-key element)))
       (return (values element index)))
     (unless (or element nil-index)
       (setf nil-index index))
     (unless (zerop (decf remaining))
       (incf index)
       (go next-element))
     (unless makep
       (return (values nil nil)))
     (when nil-index
       (incf (%hash-table-count hash-table))
       (return (values (setf (aref data nil-index) (make-entry :hash hash :key key))
                       nil-index)))
   extend-block
     (setf (aref data index) (fill-pointer data)
           index (fill-pointer data))
     (unless (<= (+ (length data) block-size) (array-total-size data))
       (adjust-array data (compute-rehash-size hash-table (array-total-size data)) :initial-element nil))
     (incf (fill-pointer data) block-size)
     (setf (aref data index) element)
     (incf index)
     (incf (%hash-table-count hash-table))
     (return (values (setf (aref data index) (make-entry :hash hash :key key))
                     index))))

(defmethod (setf salmagundi:gethash)
    (value key (hash-table block-hash-table) &optional default)
  (declare (ignore default))
  (setf (entry-value (find-entry hash-table key t)) value)
  (maybe-grow-and-rehash hash-table)
  value)

(defmethod salmagundi:gethash (key (hash-table block-hash-table) &optional default)
  (let ((entry (find-entry hash-table key)))
    (declare (type (or null entry) entry))
    (if entry
        (values (entry-value entry) t)
        (values default nil))))

(defmethod salmagundi:remhash (key (hash-table block-hash-table))
  (multiple-value-bind (entry index)
      (find-entry hash-table key)
    (declare (ignore entry)
             (type (or null fixnum) index))
    (when index
      (setf (aref (hash-table-data hash-table) index) nil)
      (decf (%hash-table-count hash-table))
      t)))

(defmethod salmagundi:clrhash ((hash-table block-hash-table))
  (with-accessors ((data hash-table-data))
      hash-table
    (fill data nil)
    (setf (%hash-table-count hash-table) 0
          (fill-pointer data) (ash (array-total-size data) -1))
    hash-table))

(defmethod salmagundi:maphash (function (hash-table block-hash-table))
  (prog* ((data (hash-table-data hash-table))
          (index (length data))
          (element nil))
  (declare (type fixnum index)
           (type element element))
   next-element
     (when (minusp (decf index))
       (return nil))
     (setf element (aref data index))
     (when (entry-p element)
       (funcall function (entry-key element) (entry-value element)))
     (go next-element)))
