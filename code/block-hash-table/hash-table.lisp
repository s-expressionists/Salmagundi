(cl:in-package #:salmagundi/block-hash-table)

(defvar *iterator-data* nil)

(defvar *iterator-index* nil)

(defclass client (salmagundi:standard-client) ())

(defstruct entry
  (hash 0 :read-only t :type fixnum)
  (key nil :read-only t)
  (value nil))

(deftype element ()
  `(or null fixnum entry))

(defclass block-hash-table (salmagundi:hash-table)
  ((data :accessor hash-table-data
         :type simple-vector)
   (limit :accessor limit
          :initform 0
          :type fixnum)
   (count :accessor %hash-table-count
          :reader salmagundi:hash-table-count
          :initform 0)
   (block-size :accessor block-size
               :initarg :block-size
               :initform 8)
   (bytespec :accessor bytespec)
   (collision-fraction :accessor collision-fraction
                       :initarg :collision-fraction
                       :initform 0.5))
  (:default-initargs :rehash-threshold 0.8
                     :rehash-size 2.0))

(defmethod initialize-instance :after ((hash-table block-hash-table) &key (size 16))
  (with-accessors ((block-size block-size)
                   (bytespec bytespec)
                   (limit limit)
                   (data hash-table-data))
      hash-table
    (setf block-size (salmagundi:ceiling-pow2 block-size)
          size (salmagundi:ceiling-pow2 size)
          limit (salmagundi:ceiling-pow2 (ceiling (- size
                                                     (* size (collision-fraction hash-table)))))
          bytespec (byte (- (integer-length limit) 1) 0)
          data (make-array size :initial-element nil))))

(defmethod salmagundi:make-hash-table ((client client) &rest initargs
                                       &key test size rehash-size rehash-threshold
                                            hash-function)
  (declare (ignore test size rehash-size rehash-threshold hash-function))
  (apply #'make-instance 'block-hash-table initargs))

(defmethod salmagundi:hash-table-size ((hash-table block-hash-table))
  (array-total-size (hash-table-data hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table block-hash-table))
  (let ((iterator-data (hash-table-data hash-table))
        (iterator-index (limit hash-table))
        (element nil))
    (declare (type simple-vector iterator-data)
             ;(type fixnum *iterator-index*)
             (type element element))
    (lambda ()
      (block hash-table-iterator
        (tagbody
         next-element
           (when (minusp (decf iterator-index))
             (return-from hash-table-iterator nil))
           (when (entry-p (setf element (svref iterator-data iterator-index)))
             (return-from hash-table-iterator
               (values t (entry-key element) (entry-value element))))
           (go next-element))))))

(defun compute-rehash-size (hash-table &optional (size (salmagundi:hash-table-size hash-table)))
  (let ((rehash-size (salmagundi:hash-table-rehash-size hash-table)))
    (salmagundi:ceiling-pow2 (+ size
                                (if (integerp rehash-size)
                                    rehash-size
                                    (ceiling (* size rehash-size)))))))

(defmethod salmagundi:rehash
    ((hash-table block-hash-table) &key (size (compute-rehash-size hash-table)))
  (prog* ((data (hash-table-data hash-table))
          (index (limit hash-table))
          (new-index 0)
          (remaining 0)
          (new-limit (salmagundi:ceiling-pow2 (ceiling (- size
                                                          (* size
                                                             (collision-fraction hash-table))))))
          (bytespec (byte (- (integer-length new-limit) 1) 0))
          (new-data (make-array size :initial-element nil))
          (block-size (block-size hash-table))
          (element nil)
          (new-element nil))
     (declare (type simple-vector data new-data)
              (type fixnum index new-index remaining block-size)
              (type element element new-element))
   next-element
     (when (minusp (decf index))
       (setf (hash-table-data hash-table) new-data
             (limit hash-table) new-limit
             (bytespec hash-table) bytespec)
       (return nil))
     (setf element (svref data index))
     (unless (entry-p element)
       (go next-element))
     (setf new-index (ldb bytespec (entry-hash element))
           new-element (svref new-data new-index))
     (when (integerp new-element)
       (go next-block))
     (unless new-element
       (setf (svref new-data new-index) element)
       (go next-element))
     (go extend-block)
   next-block
     (setf remaining block-size)
   next-new-element
     (setf new-element (svref new-data new-index))
     (when (integerp new-element)
       (setf new-index new-element)
       (go next-block))
     (unless new-element
       (setf (svref new-data new-index) element)
       (go next-element))
     (unless (zerop (decf remaining))
       (incf new-index)
       (go next-new-element))
   extend-block
     (unless (<= (+ (length new-data) block-size) (array-total-size new-data))
       (setf index (limit hash-table)
             size (compute-rehash-size hash-table size)
             new-limit (salmagundi:ceiling-pow2 (ceiling (- size
                                                            (* size
                                                               (collision-fraction hash-table)))))
             bytespec (byte (- (integer-length new-limit) 1) 0)
             new-data (make-array size :initial-element nil))
       (go next-element))
     (setf (svref new-data new-index) new-limit
           new-index new-limit)
     (incf new-limit block-size)
     (setf (svref new-data new-index) new-element)
     (incf new-index)
     (setf (svref new-data new-index) element)
     (go next-element)))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (salmagundi:hash-table-count hash-table)
           (* (salmagundi:hash-table-size hash-table)
              (salmagundi:hash-table-rehash-threshold hash-table)))
    (salmagundi:rehash hash-table)))

(declaim (ftype (function (block-hash-table t &key (:make t))
                          (values element (or null fixnum)))
                find-entry))

(defun find-entry (hash-table key &key ((:make makep) nil))
  (declare (type block-hash-table hash-table))
  (prog* ((test (salmagundi:hash-table-test hash-table))
          (hash 0)
          (index 0)
          (data (hash-table-data hash-table))
          (element nil)
          (block-size (block-size hash-table))
          (nil-index nil)
          (remaining 0))
     (declare (type (or symbol function) test)
              (type fixnum hash index remaining block-size)
              (type element element)
              (type (or null fixnum) nil-index)
              (type simple-vector data))
     (when (and (eq data *iterator-data*)
                *iterator-index*
                (entry-p (setf element (svref *iterator-data* *iterator-index*)))
                ;(= hash (entry-hash element))
                (funcall test key (entry-key element)))
       (return (values element *iterator-index*)))
     (setf hash (funcall (the (or symbol function)
                              (salmagundi:hash-table-hash-function hash-table))
                         key))
   begin
     (setf nil-index nil
           index (ldb (bytespec hash-table) hash)
           element (the element (svref data index)))
     (when (integerp element)
       (setf index element)
       (go next-block))
     (when (and (null element) makep)
       (incf (%hash-table-count hash-table))
       (return (values (setf (svref data index) (make-entry :hash hash :key key))
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
     (setf element (the element (svref data index)))
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
       (return (values (setf (svref data nil-index) (make-entry :hash hash :key key))
                       nil-index)))
   extend-block
     (unless (<= (+ (limit hash-table) block-size) (array-total-size data))
       (salmagundi:rehash hash-table)
       (setf data (hash-table-data hash-table))
       (go begin))
     (setf (svref data index) (limit hash-table)
           index (limit hash-table))
     (incf (limit hash-table) block-size)
     (setf (svref data index) element)
     (incf index)
     (incf (%hash-table-count hash-table))
     (return (values (setf (svref data index) (make-entry :hash hash :key key))
                     index))))

(defmethod (setf salmagundi:gethash)
    (value key (hash-table block-hash-table) &optional default)
  (declare (ignore default))
  (setf (entry-value (find-entry hash-table key :make t)) value)
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
      (setf (svref (hash-table-data hash-table) index) nil)
      (decf (%hash-table-count hash-table))
      t)))

(defmethod salmagundi:clrhash ((hash-table block-hash-table))
  (with-accessors ((data hash-table-data))
      hash-table
    (fill data nil)
    (setf (%hash-table-count hash-table) 0
          (limit hash-table) (ash 1 (byte-size (bytespec hash-table))))
    hash-table))

(defmethod salmagundi:maphash (function (hash-table block-hash-table))
  (prog ((*iterator-data* (hash-table-data hash-table))
         (*iterator-index* (limit hash-table))
         (element nil))
     (declare (simple-vector *iterator-data*)
              (type fixnum *iterator-index*)
              (type element element))
   next-element
     (when (minusp (decf *iterator-index*))
       (return nil))
     (setf element (svref *iterator-data* *iterator-index*))
     (when (entry-p element)
       (funcall function (entry-key element) (entry-value element)))
     (go next-element)))
