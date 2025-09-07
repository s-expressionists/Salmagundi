(cl:in-package #:salmagundi/list)

(defclass list-client (salmagundi:standard-client) ())

(defclass list-hash-table (salmagundi:hash-table)
  ((%size :accessor size
          :reader salmagundi:hash-table-size
          :initarg :size
          :initform 0)
   (%contents :accessor contents
              :initform '()))
  (:default-initargs :rehash-size 2
                     :rehash-threshold 2))

(defmethod salmagundi:make-hash-table ((client list-client)
                                       &key (test 'eql) (size 0) (rehash-size 2)
                                            (rehash-threshold 2))
  (make-instance 'list-hash-table
                 :test test :size size :rehash-size rehash-size
                 :rehash-threshold rehash-threshold))

(defmethod salmagundi:clrhash ((hash-table list-hash-table))
  (setf (contents hash-table) '()))

(defmethod salmagundi:gethash (key (hash-table list-hash-table) &optional default)
  (declare (ignore default))
  (with-accessors ((contents contents)
                   (test salmagundi:hash-table-test))
      hash-table
    (let ((entry (assoc key contents :test test)))
      (if (null entry)
          (values nil nil)
          (values (cdr entry) t)))))

(defmethod salmagundi:hash-table-count ((hash-table list-hash-table))
  (length (contents hash-table)))

(defmethod salmagundi:make-hash-table-iterator ((hash-table list-hash-table))
  (let ((contents (contents hash-table)))
    (lambda ()
      (if (null contents)
          nil
          (let ((entry (pop contents)))
            (values t (car entry) (cdr entry)))))))

(defmethod salmagundi:maphash (function (hash-table list-hash-table))
  (loop for (key . value) in (contents hash-table)
        do (funcall function key value)))

(defmethod salmagundi:remhash (key (hash-table list-hash-table))
  (with-accessors ((contents contents)
                   (test salmagundi:hash-table-test))
      hash-table
    (let ((position (position key contents :key #'car :test test)))
      (if (null position)
          nil
          (progn (setf contents
                       (nconc (subseq contents 0 position)
                              (nthcdr (1+ position) contents)))
                 t)))))

(defmethod (setf salmagundi:gethash)
    (new-value key (hash-table list-hash-table) &optional default)
  (declare (ignore default))
  (with-accessors ((contents contents)
                   (size size)
                   (test salmagundi:hash-table-test)
                   (rehash-size salmagundi:hash-table-rehash-size)
                   (rehash-threshold salmagundi:hash-table-rehash-threshold)
                   (count salmagundi:hash-table-count))
      hash-table
    (let ((entry (assoc key contents :test test)))
      (if (null entry)
          (progn
            (push (cons key new-value) contents)
            (when (> count (* rehash-threshold size))
              (if (integerp rehash-size)
                  (incf size rehash-size)
                  (setf size (ceiling (* size rehash-size))))))
          (setf (cdr entry) new-value))))
  new-value)
