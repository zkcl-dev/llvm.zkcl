(defpackage #:zk-llvm/examples/ir-pipeline
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))
(in-package #:zk-llvm/examples/ir-pipeline)

(defun print-functions (module)
  (dolist (function (llvm:module-functions module))
    (format t "~A ~S~%" (llvm:function-name function)
            (llvm:type-spec (llvm:function-signature function)))
    (loop for parameter in (llvm:function-parameters function)
          for index from 0
          for name = (llvm:value-name parameter)
          do (format t "  %~A has ~D operand uses~%"
                     (if (and name (plusp (length name))) name index)
                     (length (llvm:value-uses parameter module))))
    (dolist (block (llvm:function-blocks function))
      (format t "  ~A ~{~(~A~)~^, ~}~%"
              (llvm:block-name block)
              (mapcar #'llvm:instruction-opcode (llvm:block-instructions block))))))

(defun main ()
  (destructuring-bind (&optional input output (pipeline "default<O2>"))
      (zk:command-line-arguments)
    (unless input
      (error "Usage: zkcl run --bin ir-pipeline -- INPUT [OUTPUT [PASSES]]"))
    (llvm:with-objects ()
      (let ((module (llvm:read-module input)))
        (print-functions module)
        (when output
          (let ((optimized (llvm:optimize-module module pipeline)))
            (format t "Wrote ~A~%" (llvm:write-module optimized output))))))))
