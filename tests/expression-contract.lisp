(in-package #:zk-llvm/tests)

(defun test-tail-expression-typing ()
  (llvm:with-objects ()
    (llvm:with-jit (jit)
      (let* ((module
               (llvm:define-module (module "tail-expression-typing" :jit jit)
                 (llvm:defir sign ((positive :i32)) :i32
                   (llvm:if positive 1 -1))
                 (llvm:defir nested ((selector :i32) (input :i32)) :i64
                   (let ((fallback -4294967297))
                     (declare (special fallback))
                     (macrolet ((classify (x)
                                  `(llvm:cond
                                     ((llvm:< ,x 0) 4294967296)
                                     (:else (llvm:if (llvm:= ,x 0) 4294967297 4294967298)))))
                       (llvm:switch selector
                         (0 (classify input))
                         (:default fallback)))))
                 (llvm:defir terminated-test ((selector :i32)) :i32
                   (llvm:cond
                     ((llvm:= selector 0) 41)
                     ((llvm:ret 42))))
                 (llvm:defir typed-storage ((choice :i32)) :i32
                   (let ((slot (llvm:alloca :i64)))
                     (llvm:store (llvm:if choice 4294967297 4294967298 :type :i64)
                                 slot :type :i64)
                     (llvm:int-cast (llvm:load :i64 slot) :i32)))))
             (ir (llvm:module-ir module)))
        (assert (search "phi i32" ir))
        (assert (search "phi i64" ir))
        (llvm:jit-add-module jit module)
        (let ((sign (llvm:jit-bind jit 'sign :i32 :i32))
              (nested (llvm:jit-bind jit 'nested :i64 :i32 :i32))
              (terminated-test (llvm:jit-bind jit 'terminated-test :i32 :i32))
              (typed-storage (llvm:jit-bind jit 'typed-storage :i32 :i32)))
          (assert (= 1 (funcall sign 1)))
          (assert (= -1 (funcall sign 0)))
          (assert (= 4294967296 (funcall nested 0 -1)))
          (assert (= 4294967297 (funcall nested 0 0)))
          (assert (= 4294967298 (funcall nested 0 1)))
          (assert (= -4294967297 (funcall nested 1 0)))
          (assert (= 41 (funcall terminated-test 0)))
          (assert (= 42 (funcall terminated-test 1)))
          (assert (= 1 (funcall typed-storage 1)))
          (assert (= 2 (funcall typed-storage 0))))))))

(defun test-arm-result-boundaries ()
  (flet ((fails-with (text body)
           (llvm:with-objects ()
             (llvm:with-module (module "invalid-arm")
               (assert (handler-case (progn (funcall body) nil)
                         (error (condition) (search text (princ-to-string condition)))))))))
    (fails-with "yield only literals"
      (lambda ()
        (llvm:defir comparison ((flag :i32)) :i1
          (llvm:= (llvm:if flag 300 1) (llvm:const :i32 300)))))
    (fails-with "yield only literals"
      (lambda ()
        (llvm:defir store-value ((flag :i32)) :i64
          (llvm:store (llvm:if flag 300 1) (llvm:alloca :i32) :type :i32))))
    (fails-with "yield only literals"
      (lambda ()
        (llvm:defir discarded-arm ((flag :i32)) :i64
          (llvm:if flag
            (llvm:if flag 300 1)
            :type :void))))
    (dolist (body (list (lambda ()
                         (llvm:defir mixed-if ((flag :i32)) :void
                           (llvm:if flag 1 (values))))
                       (lambda ()
                         (llvm:defir mixed-cond ((flag :i32)) :void
                           (llvm:cond (flag 1) (:else (values)))))
                       (lambda ()
                         (llvm:defir mixed-switch ((flag :i32)) :void
                           (llvm:switch flag (0 1) (:default (values)))))))
      (fails-with "continuing arms with and without values" body))))

(defun test-statement-expression-contract ()
  (llvm:with-objects ()
    (let* ((events nil)
           (module
             (llvm:define-module (module "statement-expression-contract")
               (llvm:defir effects ((selector :i32)) :i32
                 (let ((slot (llvm:alloca :i32 0)))
                   (assert (null (multiple-value-list
                                  (llvm:if (progn (push :test events) selector)
                                    (progn
                                      (push :arm events)
                                      (llvm:store 1 slot :type :i32)
                                      40)
                                    :type (progn (push :type events) :void)))))
                   (assert (null (multiple-value-list
                                  (llvm:if (llvm:= selector 2)
                                    (progn (llvm:store 2 slot :type :i32) 50)))))
                   (assert (null (multiple-value-list
                                  (llvm:cond
                                    ((llvm:= selector 3)
                                     (llvm:store 3 slot :type :i32)
                                     60)))))
                   (assert (null (multiple-value-list
                                  (llvm:switch selector
                                    (4 (llvm:store 4 slot :type :i32) 70)))))
                   (assert (null (multiple-value-list
                                  (llvm:if selector (values) (values)))))
                   (llvm:load :i32 slot))))))
      (assert (equal '(:test :type :arm) (reverse events)))
      (assert (null (search "phi " (llvm:module-ir module))))
      (llvm:with-jit (jit)
        (llvm:jit-add-module jit module)
        (let ((effects (llvm:jit-bind jit 'effects :i32 :i32)))
          (dotimes (index 5)
            (assert (= index (funcall effects index)))))))))

(defun test-default-optimization-pipeline-options ()
  (llvm:with-objects ()
    (let ((module (llvm:define-module (module "optimization-pipeline-options")
                    (llvm:defir answer () :i32 42)))
          (options (llvm:make-pass-options :verify-each t))
          (machine (llvm:make-target-machine)))
      (dolist (result (list (llvm:optimize-module module :options options)
                            (llvm:optimize-module module :target-machine machine :options options)
                            (llvm:optimize-module module :o1 :options options)
                            (llvm:optimize-module module "default<O0>")))
        (assert (llvm:verify-module result))))))

(defun test-call-literal-arguments ()
  (llvm:with-objects ()
    (let ((module
            (llvm:define-module (module "call-literals")
              (let ((prototype (llvm:declare-ir variadic (:i64 :i32) :i64 :variadic t)))
                (llvm:defir direct ((value :i32)) :i64
                  (assert (handler-case
                              (progn (llvm:call prototype 4294967297 value 7) nil)
                            (llvm:llvm-error (condition)
                              (and (eq 'llvm:call (llvm:llvm-error-operation condition))
                                   (search "Variadic argument 3" (llvm:llvm-error-message condition))))))
                  (llvm:call prototype 4294967297 value (llvm:const :i16 7)))
                (llvm:defir indirect ((target :ptr) (value :i32)) :i64
                  (llvm:indirect-call prototype target 4294967297 value (llvm:const :i16 7)))))))
      (dolist (name '("direct" "indirect"))
        (let* ((function (llvm:find-function module name))
               (entry (first (llvm:function-blocks function)))
               (operands (llvm:call-arguments (llvm:block-first-instruction entry))))
          (assert (equal '(64 32 16)
                         (mapcar (lambda (operand) (llvm:integer-width (llvm:value-type operand)))
                                 operands)))
          (assert (= 4294967297 (llvm:constant-integer (first operands))))
          (assert (= 7 (llvm:constant-integer (third operands)))))))))

(defun test-expression-contract ()
  (test-tail-expression-typing)
  (test-arm-result-boundaries)
  (test-statement-expression-contract)
  (test-default-optimization-pipeline-options)
  (test-call-literal-arguments))
