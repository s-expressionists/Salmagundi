(cl:in-package #:salmagundi)

(defun check-hash-table (object)
  (unless (hash-table-p object)
    (error 'type-error :datum object :expected-type 'hash-table)))

(trinsic:make-define-interface (:intrinsic intrinsicp :client-form client-var)
    ((make-hash-table-sym cl:make-hash-table)
     (with-hash-table-iterator-sym cl:with-hash-table-iterator))
   (let* ((intrinsic-pkg (if intrinsicp "COMMON-LISP" (package-name *package*))))
     `((shadowing-import '(clrhash
                           gethash
                           hash-table
                           hash-table-count
                           hash-table-p
                           hash-table-rehash-size
                           hash-table-rehash-threshold
                           hash-table-size
                           hash-table-test
                           remhash)
                         ,intrinsic-pkg)

       (export '(clrhash
                 gethash
                 hash-table
                 hash-table-count
                 hash-table-p
                 hash-table-rehash-size
                 hash-table-rehash-threshold
                 hash-table-size
                 hash-table-test
                 remhash)
               ,intrinsic-pkg)

       (defun ,make-hash-table-sym
           (&rest rest
            &key test size rehash-size rehash-threshold
            &allow-other-keys)
         (declare (ignore test size rehash-size rehash-threshold))
         (apply #'make-hash-table ,client-var rest))

       (defmacro ,with-hash-table-iterator-sym
           ((name hash-table) &body body)
         (let ((iterator-var (gensym "ITERATOR")))
           `(let ((,iterator-var (make-hash-table-iterator ,hash-table)))
              (macrolet ((,name () `(funcall ,',iterator-var)))
                ,@body)))))))
