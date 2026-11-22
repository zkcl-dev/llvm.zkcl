(defpackage #:zk-llvm/examples/rewrite-ir
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))
(in-package #:zk-llvm/examples/rewrite-ir)

(defparameter *input-module*
  #.(namestring (merge-pathnames "positive-part.ll"
                                 (or *compile-file-truename* *load-truename*))))

(defun clamp-results (module)
  (llvm:with-jit (jit)
    (llvm:with-jit-module (jit module)
      (llvm:with-jit-binding (clamp jit "positive_part" :i64 :i64)
        (mapcar clamp '(-7 8 20))))))

(defun change-floor (module floor)
  (let* ((function (llvm:find-function module "positive_part"))
         (result (llvm:find-instruction function "answer"))
         (return (llvm:instruction-next result)))
    (llvm:with-module-edit (edit module)
      (llvm:with-edit-builder (edit :before return)
        (let* ((value (llvm:draft-value edit (llvm:instruction-result result)))
               (clamped (llvm:select (llvm:< value floor) floor value "clamped")))
          (llvm:edit-replace-operand edit return 0 clamped))))))

(defun main ()
  (destructuring-bind (&optional (input *input-module*))
      (zk:command-line-arguments)
    (llvm:with-objects ()
      (let* ((module (llvm:read-module input))
             (rewritten (change-floor module 10))
             (original (clamp-results module))
             (updated (clamp-results rewritten)))
        (assert (equal original '(0 8 20)))
        (assert (equal updated '(10 10 20)))
        (format t "Input     ~{~D~^, ~}~%Floor 0   ~{~D~^, ~}~%Floor 10  ~{~D~^, ~}~%"
                '(-7 8 20) original updated)))))
