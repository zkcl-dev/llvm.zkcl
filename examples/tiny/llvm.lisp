;;;; Lower the checked tree to LLVM SSA values and assemble a verified module.

(in-package #:zk-llvm/examples/compiler)

(defun lower-type (type)
  (zk:match type
    ((list 'pair left right) (list :struct (lower-type left) (lower-type right)))
    (_ type)))

(defun emit-expression (expression variables)
  (declare (type checked-expression expression))
  (flet ((emit (subexpression) (emit-expression subexpression variables)))
    (zk:match expression
      ((literal type value) (llvm:const type value))
      ((reference name) (cdr (assoc name variables)))
      ((binary operator left right)
       (funcall (ecase operator (+ #'llvm:+) (- #'llvm:-) (* #'llvm:*) (< #'llvm:<))
                (emit left) (emit right)))
      ((binding name initializer body)
       (emit-expression body (acons name (emit initializer) variables)))
      ((branch type test consequent alternative)
       ;; Each arm emits into a different block. LLVM:IF joins their values with a PHI.
       (llvm:if (emit test) (emit consequent) (emit alternative) :type (lower-type type)))
      ((folding type index start end accumulator initial body)
       (let ((first (emit start)) (limit (emit end)) (seed (emit initial))
             (ir-type (lower-type type)))
         ;; LLVM:LOOP builds loop-carried PHIs for both locals. Position is
         ;; incremented only when below the signed limit, so it cannot overflow.
         (llvm:loop ((position :i64 first) (value ir-type seed)) ir-type
           (llvm:if (llvm:< position limit)
               (llvm:recur (llvm:+ position 1)
                           (emit-expression body
                                            (acons index position
                                                   (acons accumulator value variables))))
               (llvm:break value)))))
      ((pair-value type left right)
       ;; INSERT returns an SSA aggregate. Both fields are set before it is used.
       (llvm:insert (llvm:insert (llvm:const-undef (lower-type type)) (emit left) 0)
                    (emit right) 1))
      ((projection index pair) (llvm:extract (emit pair) index))
      ((invocation name arguments) (apply #'llvm:call name (mapcar #'emit arguments))))))

(defun declare-signature (module signature)
  (llvm:declare-function module (function-signature-name signature)
                         (lower-type (function-signature-result signature))
                         (mapcar (lambda (parameter) (lower-type (cdr parameter)))
                                 (function-signature-parameters signature))))

(defun emit-definition (function definition)
  (let* ((signature (checked-function-signature definition))
         (names (mapcar #'car (function-signature-parameters signature))))
    (llvm:emit-function-body
     function (mapcar #'symbol-name names)
     (lambda (parameters)
       (llvm:ret (emit-expression (checked-function-body definition)
                                  (mapcar #'cons names parameters)))))))

(defun compile-program (definitions &key imports jit target-machine optimize)
  (llvm:define-module (module "tiny-language" :jit jit :target-machine target-machine
                        :optimize optimize)
    ;; Declare every function first, so forward and mutually recursive calls resolve.
    (dolist (source imports)
      (declare-signature module (source-function-signature source)))
    (let ((functions (loop for definition in definitions
                           collect (declare-signature module (checked-function-signature definition)))))
      (mapc #'emit-definition functions definitions))))

(defun call-jit (jit name result-type parameter-types arguments)
  (multiple-value-bind (function release) (apply #'llvm:jit-bind jit name result-type parameter-types)
    (unwind-protect (apply function arguments)
      (funcall release))))
