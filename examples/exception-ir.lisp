(defpackage #:zk-llvm/examples/exception-ir
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))
(in-package #:zk-llvm/examples/exception-ir)

(defun read-and-release-module ()
  (llvm:define-module (module "read-and-release")
    (let ((personality (llvm:declare-ir "__gcc_personality_v0" () :i32 :variadic t))
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
          (let ((exception (llvm:landing-pad '(:struct :ptr :i32) :cleanup t)))
            (llvm:call release-buffer buffer)
            (llvm:resume exception)))))))

(defun main ()
  (llvm:with-objects ()
    (write-string (llvm:module-ir (read-and-release-module)))))
