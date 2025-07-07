(cl:in-package #:salmagundi)

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defun check-hash-table (object)
  (unless (hash-table-p object)
    (error 'type-error :datum object :expected-type 'hash-table)))

(defmacro define-interface (client-var client-class &optional intrinsic)
  (declare (ignore client-class))
  (let* ((pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (clrhash-sym (ensure-symbol '#:clrhash pkg))
         (gethash-sym (ensure-symbol '#:gethash pkg))
         (hash-table-sym (ensure-symbol '#:hash-table pkg))
         (hash-table-count-sym (ensure-symbol '#:hash-table-count pkg))
         (hash-table-p-sym (ensure-symbol '#:hash-table-p pkg))
         (hash-table-rehash-size-sym (ensure-symbol '#:hash-table-rehash-size pkg))
         (hash-table-rehash-threshold-sym (ensure-symbol '#:hash-table-rehash-threshold pkg))
         (hash-table-size-sym (ensure-symbol '#:hash-table-size pkg))
         (hash-table-test-sym (ensure-symbol '#:hash-table-ters pkg))
         (remhash-sym (ensure-symbol '#:remhash pkg))
         (make-hash-table-sym (ensure-symbol '#:make-hash-table pkg))
         (with-hash-table-iterator-sym (ensure-symbol '#:with-hash-table-iterator pkg)))

    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (shadowing-import '(clrhash
                             gethash
                             hash-table
                             hash-table-count
                             hash-table-p
                             hash-table-rehash-size
                             hash-table-rehash-threshold
                             hash-table-size
                             hash-table-test
                             remhash)
                           ,(package-name pkg)))
       
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
                ,@body))))
       
       (export '(,clrhash-sym
                 ,gethash-sym
                 ,hash-table-sym
                 ,hash-table-count-sym
                 ,hash-table-p-sym
                 ,hash-table-rehash-size-sym
                 ,hash-table-rehash-threshold-sym
                 ,hash-table-size-sym
                 ,hash-table-test-sym
                 ,make-hash-table-sym
                 ,remhash-sym
                 ,with-hash-table-iterator-sym)))))


