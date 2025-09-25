(cl:in-package #:salmagundi/jump-hash-table)

(defvar *iterator-data* nil)

(defvar *iterator-index* nil)

(defclass client (salmagundi:standard-client) ())

(defstruct entry
  (hash 0 :read-only t :type fixnum)
  (key nil :read-only t)
  (value nil))

(deftype element ()
  `(or null fixnum entry))

(declaim (ftype (function (hash-table t &optional t)
                          (values (or null entry) (or null fixnum)))
                find-entry))

(defclass hash-table (salmagundi:hash-table)
  ((%data :accessor hash-table-data
          :type simple-vector)
   (%limit :accessor limit
           :initform 0
           :type fixnum)
   (%count :accessor %hash-table-count
           :reader salmagundi:hash-table-count
           :initform 0)
   (%block-size :accessor block-size
                :initarg :block-size
                :initform 4)
   (%bytespec :accessor bytespec)
   (%collision-fraction :accessor collision-fraction
                        :initarg :collision-fraction
                        :initform 0.5))
  (:default-initargs :rehash-threshold 0.8
                     :rehash-size 2.0))

(defmethod initialize-instance :after ((hash-table hash-table) &key (size 16))
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
  (apply #'make-instance 'hash-table initargs))

(defmethod salmagundi:hash-table-size ((hash-table hash-table))
  (length (hash-table-data hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table hash-table))
  (let ((iterator-data (hash-table-data hash-table))
        (iterator-index (limit hash-table))
        (element nil))
    (declare (type simple-vector iterator-data)
             (type fixnum iterator-index)
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
    ((hash-table hash-table) &key (size (compute-rehash-size hash-table)))
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
     (typecase (setf element (the element (svref data index)))
       (entry
        (setf new-index (ldb bytespec (entry-hash element))
              new-element (svref new-data new-index))
        (when (integerp new-element)
          (go next-block))
        (unless new-element
          (setf (svref new-data new-index) element)
          (go next-element))
        (go extend-block))
       (t
        (go next-element)))
   next-block
     (setf remaining block-size)
   next-new-element
     (setf new-element (the element (svref new-data new-index)))
     (when (typep new-element 'fixnum)
       (setf new-index new-element)
       (go next-block))
     (unless new-element
       (setf (svref new-data new-index) element)
       (go next-element))
     (unless (zerop (decf remaining))
       (incf new-index)
       (go next-new-element))
   extend-block
     (unless (<= (+ new-limit block-size) (length new-data))
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

(defun find-entry (hash-table key &optional makep)
  (declare (type hash-table hash-table))
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
                (funcall test key (entry-key element)))
       (return (values element *iterator-index*)))
     (setf hash (funcall (the (or symbol function)
                              (salmagundi:hash-table-hash-function hash-table))
                         key))
   begin
     (setf nil-index nil
           index (ldb (bytespec hash-table) hash))
     (typecase (setf element (the element (svref data index)))
       (fixnum
        (setf index element)
        (go next-block))
       (entry
        (when (and (= hash (entry-hash element))
                   (funcall test key (entry-key element)))
          (return (values element index)))
        (unless makep
          (return (values nil nil)))
        (go extend-block))
       (t
        (unless makep
          (return nil))
        (incf (%hash-table-count hash-table))
        (return (values (setf (svref data index) (make-entry :hash hash :key key))
                        index))))
   next-block
     (setf remaining block-size)
   next-element
     (typecase (setf element (the element (svref data index)))
       (fixnum
        (go next-block))
       (entry
        (when (and (= hash (entry-hash element))
                   (funcall test key (entry-key element)))
          (return (values element index))))
       (t
        (unless nil-index
          (setf nil-index index))))
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
     (unless (<= (+ (limit hash-table) block-size) (length data))
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
    (value key (hash-table hash-table) &optional default)
  (declare (ignore default))
  (setf (entry-value (the entry (find-entry hash-table key t))) value)
  (maybe-grow-and-rehash hash-table)
  value)

(defmethod salmagundi:gethash (key (hash-table hash-table) &optional default)
  (let ((entry (find-entry hash-table key)))
    (declare (type (or null entry) entry))
    (if (entry-p entry)
        (values (entry-value entry) t)
        (values default nil))))

(defmethod salmagundi:remhash (key (hash-table hash-table))
  (multiple-value-bind (entry index)
      (find-entry hash-table key)
    (declare (ignore entry)
             (type (or null fixnum) index))
    (when index
      (setf (svref (hash-table-data hash-table) index) nil)
      (decf (%hash-table-count hash-table))
      t)))

(defmethod salmagundi:clrhash ((hash-table hash-table))
  (fill (hash-table-data hash-table) nil)
  (setf (%hash-table-count hash-table) 0
        (limit hash-table) (ash 1 (byte-size (bytespec hash-table))))
  hash-table)

(defmethod salmagundi:maphash (function (hash-table hash-table))
  (prog ((*iterator-data* (hash-table-data hash-table))
         (*iterator-index* (limit hash-table))
         (element nil))
     (declare (simple-vector *iterator-data*)
              (type fixnum *iterator-index*)
              (type element element))
   next-element
     (when (minusp (decf *iterator-index*))
       (return nil))
     (when (entry-p (setf element (the element (svref *iterator-data* *iterator-index*))))
       (funcall function (entry-key element) (entry-value element)))
     (go next-element)))
