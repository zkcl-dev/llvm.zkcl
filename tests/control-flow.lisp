(in-package #:zk-llvm/tests)

(defun test-control-form-evaluation-order ()
  (llvm:with-objects ()
    (let ((events nil))
      (llvm:define-module (module "control-form-evaluation")
        (llvm:defir if-order () :i32
          (llvm:if (progn (push :test events) (llvm:const :i1 t))
            1 2 :type (progn (push :type events) :i32)))
        (assert (equal '(:test :type) (reverse events)))
        (setf events nil)
        (llvm:defir switch-order () :i32
          (llvm:switch (progn (push :selector events) (llvm:const :i32 0))
            (:type (progn (push :type events) :i32))
            (0 1)
            (:default 2)))
        (assert (equal '(:selector :type) (reverse events)))
        (llvm:defir if-stops () :i32
          (llvm:if (llvm:ret 3)
            (error "Emitted an IF arm after return.")
            (error "Emitted an ELSE arm after return.")
            :type (error "Evaluated IF type after return.")))
        (llvm:defir switch-stops () :i32
          (llvm:switch (llvm:ret 4)
            (:type (error "Evaluated SWITCH type after return."))
            (:default (error "Emitted a SWITCH arm after return."))))))))

(defun test-void-control-flow ()
  (llvm:with-llvm-objects ()
    (let* ((module
             (llvm:define-module (m "void-control-flow")
               (llvm:defir effects ((x :i32)) :i32
                 (let ((slot (llvm:alloca :i32 0)))
                   (assert (null (multiple-value-list
                                  (llvm:if (llvm:= x 0) 8 9 :type :void))))
                   (assert (null (multiple-value-list
                                  (llvm:if (llvm:< x 0)
                                    (progn (llvm:store (llvm:const :i32 1) slot) 99)
                                    (llvm:store (llvm:const :i32 2) slot)
                                    :type :void))))
                   (assert (null (multiple-value-list
                                  (llvm:cond (:type :void)
                                    ((llvm:= x 0) (llvm:store (llvm:const :i32 3) slot))
                                    (:else 17)))))
                   (assert (null (multiple-value-list
                                  (llvm:switch x (:type :void)
                                    (1 (llvm:store (llvm:const :i32 4) slot))
                                    (:default (values))))))
                   (llvm:load :i32 slot)))))
           (ir (llvm:module-ir module)))
      (assert (not (search "phi void" ir)))
      (llvm:with-jit (jit)
        (llvm:jit-add-module jit module)
        (let ((effects (llvm:jit-bind jit "effects" :i32 :i32)))
          (assert (= 1 (funcall effects -1)))
          (assert (= 3 (funcall effects 0)))
          (assert (= 4 (funcall effects 1)))
          (assert (= 2 (funcall effects 2))))))))

(defun test-terminated-control-tests ()
  (llvm:with-llvm-objects ()
    (let ((module
            (llvm:define-module (m "terminated-control-tests")
              (llvm:defir if-test () :i32
                (llvm:if (llvm:build-ret llvm:*builder* (llvm:const :i32 11))
                  (error "Emitted an IF arm after a terminated test.")
                  (error "Emitted an ELSE arm after a terminated test."))
                (error "Emitted a continuation after a terminated IF test."))
              (llvm:defir switch-test () :i32
                (llvm:switch (llvm:ret 12)
                  (0 (error "Emitted a SWITCH arm after a terminated selector.")))
                (error "Emitted a continuation after a terminated SWITCH selector."))
              (llvm:defir cond-test ((x :i32)) :i32
                (llvm:cond (:type :i32)
                  ((llvm:< x 0) 14)
                  ((llvm:ret 15) (error "Emitted a terminated COND test arm."))
                  (:else (error "Emitted a clause after a terminated COND test."))))
              (llvm:defir while-test () :i32
                (llvm:while (llvm:ret 16)
                  (error "Emitted a WHILE body after a terminated test."))
                (error "Emitted a continuation after a terminated WHILE test."))
              (llvm:defir loop-initializer () :i32
                (llvm:loop ((i :i32 (progn
                                     (llvm:build-ret llvm:*builder* (llvm:const :i32 13))
                                     0))
                            (j :i32 (error "Emitted a later initializer after a return.")))
                    :i32
                  (llvm:break (llvm:+ i j)))
                (error "Emitted a continuation after a terminated initializer.")))))
      (llvm:with-jit (jit)
        (llvm:jit-add-module jit module)
        (assert (= 11 (funcall (llvm:jit-bind jit "if_test" :i32))))
        (assert (= 12 (funcall (llvm:jit-bind jit "switch_test" :i32))))
        (let ((cond-test (llvm:jit-bind jit "cond_test" :i32 :i32)))
          (assert (= 14 (funcall cond-test -1)))
          (assert (= 15 (funcall cond-test 0))))
        (assert (= 16 (funcall (llvm:jit-bind jit "while_test" :i32))))
        (assert (= 13 (funcall (llvm:jit-bind jit "loop_initializer" :i32))))))))

(defun test-ssa-loop-initializers ()
  (assert (handler-case
              (progn
                (macroexpand-1 '(llvm:loop ((i :i32 0) (i :i32 1)) :void
                                 (llvm:break)))
                nil)
            (error (condition)
              (search "must be distinct" (princ-to-string condition)))))
  (llvm:with-llvm-objects ()
    (assert (handler-case
                (progn
                  (llvm:define-module (m "void-loop-state")
                    (llvm:defir invalid () :void
                      (llvm:loop ((i :void 0)) :void (llvm:break))))
                  nil)
              (error (condition)
                (search "must have a value type" (princ-to-string condition)))))
    (let ((module
            (llvm:define-module (m "ssa-loop-initializers")
              (llvm:defir rotate ((x :i32)) :i32
                ;; Each initializer can create blocks. The loop's initial PHI
                ;; predecessor is the final initializer's current block.
                (llvm:loop ((a :i32 (llvm:if (llvm:< x 0) (llvm:- x) x))
                            (b :i32 (llvm:if (llvm:= x 0) 2 3 :type :i32))
                            (i :i32 0))
                    :i32
                  (llvm:if (llvm:= i 1)
                    (llvm:break (llvm:- a b))
                    ;; Updates are parallel: B receives old A.
                    (llvm:recur b a (llvm:+ i 1))))))))
      (llvm:with-jit (jit)
        (llvm:jit-add-module jit module)
        (let ((rotate (llvm:jit-bind jit "rotate" :i32 :i32)))
          (assert (= -4 (funcall rotate -7)))
          (assert (= 2 (funcall rotate 0)))
          (assert (= -4 (funcall rotate 7))))))))
