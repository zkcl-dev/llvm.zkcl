(in-package #:zk-llvm/tests)

(defun test-positive-part-property (dsl explicit)
  (let ((minimum (- (ash 1 63)))
        (maximum (1- (ash 1 63))))
    (dolist (input (list minimum (1+ minimum) -1 0 1 (1- maximum) maximum))
      (assert (= (max input 0) (funcall dsl input) (funcall explicit input))))
    (zk-pbt:forall
     (zk-pbt:integers :min minimum :max maximum)
     (lambda (input)
       (let* ((expected (max input 0))
              (dsl-result (funcall dsl input))
              (explicit-result (funcall explicit input)))
         (zk-pbt:claim :dsl (= expected dsl-result) (list expected dsl-result))
         (zk-pbt:claim :explicit-ir (= expected explicit-result)
                       (list expected explicit-result))))
     :name 'positive-part-agrees-with-lisp :seed 42064 :trials 100)))

(defun assert-positive-part-released (function)
  (assert (handler-case (progn (funcall function 1) nil)
            (error (condition) (search "released" (princ-to-string condition))))))

(defun assert-positive-part-unloaded (jit)
  (assert (handler-case
              (progn (llvm:release (llvm:jit-lookup jit "positive_part")) nil)
            (llvm:llvm-error (condition)
              (eq 'llvm:jit-lookup (llvm:llvm-error-operation condition))))))

(defun test-quickstart ()
  (llvm:with-objects ()
    (let ((module (zk-llvm/examples/explicit-ir::positive-part-module)))
      (llvm:with-jit (jit)
        (multiple-value-bind (dsl release)
            (llvm:jit-fn positive-part ((x :i64)) :i64
              (llvm:if (llvm:> x 0) x 0))
          (unwind-protect
               (multiple-value-bind (result completed)
                   (llvm:with-jit-module (jit module)
                     (multiple-value-bind (escaped result)
                         (llvm:with-jit-binding (explicit jit "positive_part" :i64 :i64)
                           (test-positive-part-property dsl explicit)
                           (values explicit (funcall explicit 8)))
                       (assert-positive-part-released escaped)
                       (values result :completed)))
                 (assert (= 8 result))
                 (assert (eq completed :completed)))
            (funcall release)))
        (assert-positive-part-unloaded jit)

        (assert (eq :submitted
                    (llvm:with-jit-module (jit module :on-demand t)
                      :submitted)))
        (assert-positive-part-unloaded jit)
        (let ((escaped
                (catch 'leave-jit-module
                  (llvm:with-jit-module (jit module :on-demand t)
                    (llvm:with-jit-binding (explicit jit "positive_part" :i64 :i64)
                      (assert (= 8 (funcall explicit 8)))
                      (throw 'leave-jit-module explicit))))))
          (assert-positive-part-released escaped))
        (assert-positive-part-unloaded jit)))))
