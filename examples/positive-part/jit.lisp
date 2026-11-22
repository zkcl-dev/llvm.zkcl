(defpackage #:zk-llvm/examples/first-jit
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))
(in-package #:zk-llvm/examples/first-jit)

(defun main ()
  (multiple-value-bind (positive-part release)
      (llvm:jit-fn positive-part ((x :i64)) :i64
        (llvm:if (llvm:> x 0) x 0))
    (unwind-protect
         (loop for (input expected) in '((-7 0) (8 8))
               for result = (funcall positive-part input)
               do (assert (= result expected))
                  (format t "positive_part(~D) = ~D~%" input result))
      (funcall release))))
