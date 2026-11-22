(defpackage #:zk-llvm/examples/dispatch-table
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))

(in-package #:zk-llvm/examples/dispatch-table)

(defun build-calibration-module (jit)
  (llvm:define-module (module "calibration" :jit jit)
    ;; Board revisions A and B have different gains and offsets.
    (let* ((board-a (llvm:defir (board-a :linkage :internal) ((sample :i64)) :i64
                      (llvm:+ (llvm:* sample 2) 2)))
           (board-b (llvm:defir (board-b :linkage :internal) ((sample :i64)) :i64
                      (llvm:+ (llvm:* sample 3) 5)))
           (table-type '(:array :ptr 2))
           (table (llvm:defglobal "calibrators" table-type :constant t :linkage :private
                    :initializer (list (llvm:function-address board-a)
                                       (llvm:function-address board-b)))))
      (llvm:defir "decode_millivolts" ((revision :i32) (sample :i32)) :i64
        (let ((reading (llvm:int-cast sample :i64)))
          (llvm:switch revision
            (0 reading)  ; The board already reports millivolts.
            ;; Only revisions 1 and 2 can reach the table access.
            ((1 2)
             (let* ((index (llvm:- revision 1))
                    (slot (llvm:gep table-type table (list 0 index)))
                    (calibrate (llvm:load :ptr slot)))
               (llvm:indirect-call board-a calibrate reading)))
            (:default -1)))))))

(defun main ()
  (llvm:with-objects ()
    (llvm:with-jit (jit)
      (llvm:with-jit-module (jit (build-calibration-module jit))
        (llvm:with-jit-binding (decode jit "decode_millivolts" :i64 :i32 :u32)
          (loop for (revision sample expected) in '((0 42 42) (1 20 42) (2 20 65) (-1 20 -1) (3 20 -1))
                for result = (funcall decode revision sample)
                do (assert (= result expected))
                   (format t "decode_millivolts(~D, ~D) = ~D~%" revision sample result)))))))
