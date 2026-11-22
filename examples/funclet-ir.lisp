(defpackage #:zk-llvm/examples/funclet-ir
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main #:read-and-release-module))
(in-package #:zk-llvm/examples/funclet-ir)

(defun read-and-release-module ()
  (llvm:define-module (module "windows-cleanup" :triple "x86_64-pc-windows-msvc")
    (let ((personality (llvm:declare-ir "__CxxFrameHandler3" () :i32 :variadic t))
          (read-sensor (llvm:declare-ir read-sensor (:ptr) :i32))
          (release-buffer (llvm:declare-ir release-buffer (:ptr) :void)))
      (llvm:defir read-and-release ((buffer :ptr)) :i32
        (llvm:set-function-personality llvm:*function* personality)
        (let* ((normal (llvm:append-block llvm:*function* "normal"))
               (unwind (llvm:append-block llvm:*function* "unwind"))
               (reading (llvm:invoke-call read-sensor normal unwind buffer)))
          (llvm:position-at-end llvm:*builder* normal)
          (llvm:call release-buffer buffer)
          (llvm:build-ret llvm:*builder* (llvm:instruction-result reading))

          (llvm:position-at-end llvm:*builder* unwind)
          (let ((pad (llvm:cleanup-pad :name "release")))
            (llvm:with-operand-bundles
                ((llvm:operand-bundle "funclet" (llvm:instruction-result pad)))
              (llvm:call release-buffer buffer))
            (llvm:cleanup-ret pad)))))))

(defun main ()
  (llvm:with-objects ()
    (write-string (llvm:module-ir (read-and-release-module)))))
