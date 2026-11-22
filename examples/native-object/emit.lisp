(defpackage #:zk-llvm/examples/native-object
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))

(in-package #:zk-llvm/examples/native-object)

;; Record the Lisp source path even when the project binary runs from a FASL.
(defparameter *source-path*
  #.(namestring (or *compile-file-truename* *load-truename*)))

(defun build-order-module (target)
  (llvm:define-module (module "orders" :target-machine target)
    (llvm:with-debug-info (debug module (file-namestring *source-path*)
                            :directory (directory-namestring *source-path*))
      ;; Match struct Order in driver.c. GEP uses the target's layout.
      (let ((order-type (llvm:struct-type '(:i32 :i64 :i32))))
        (llvm:defir ("order_total"
                     :attributes `((0 "align" ,(llvm:target-abi-alignment target order-type))
                                   (0 "dereferenceable" ,(llvm:target-abi-size target order-type))))
            ((order :ptr)) :i64
          ;; The DWARF line table points to the DSL forms that emit the code.
          (let ((scope (llvm:attach-function-debug-info debug llvm:*function* 18)))
            (llvm:with-debug-location (llvm:*builder* (llvm:make-debug-location scope 25))
              (let ((quantity (llvm:load :i32 (llvm:gep order-type order '(0 0))))
                    (price (llvm:load :i64 (llvm:gep order-type order '(0 1))))
                    (shipping (llvm:load :i32 (llvm:gep order-type order '(0 2)))))
                (llvm:with-debug-location (llvm:*builder* (llvm:make-debug-location scope 30))
                  (llvm:ret
                   (llvm:+ (llvm:* (llvm:int-cast quantity :i64) price)
                           (llvm:int-cast shipping :i64))))))))))))

(defun main ()
  (let ((arguments (zk:command-line-arguments)))
    (unless (<= 1 (length arguments) 2)
      (error "Usage: zkcl run --bin native-object -- OUTPUT [TRIPLE]"))
    (destructuring-bind (output &optional (triple (llvm:host-triple))) arguments
      (llvm:with-objects ()
        (let* ((target (llvm:make-target-machine :triple triple :optimization :none))
               (module (build-order-module target)))
          (llvm:write-module module output :format :object :target-machine target)
          (format t "Wrote ~A for ~A.~%" output (llvm:target-triple target)))))))
