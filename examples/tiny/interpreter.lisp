;;;; Reference semantics for the checked tree, using Lisp values.

(in-package #:zk-llvm/examples/compiler)

(defun wrap-i64 (integer)
  (let ((bits (ldb (byte 64 0) integer)))
    (if (logbitp 63 bits) (- bits (ash 1 64)) bits)))

(defun interpret-expression (expression variables definitions)
  (declare (type checked-expression expression))
  (flet ((interpret (subexpression)
           (interpret-expression subexpression variables definitions)))
    (zk:match expression
      ((literal _ value) value)
      ((reference name) (cdr (assoc name variables)))
      ((binary operator left right)
       (let ((left (interpret left)) (right (interpret right)))
         (ecase operator
           (+ (wrap-i64 (+ left right)))
           (- (wrap-i64 (- left right)))
           (* (wrap-i64 (* left right)))
           (< (< left right)))))
      ((binding name initializer body)
       (interpret-expression body (acons name (interpret initializer) variables) definitions))
      ((branch _ test consequent alternative)
       (interpret (if (interpret test) consequent alternative)))
      ((folding _ index start end accumulator initial body)
       (let ((first (interpret start)) (limit (interpret end)) (value (interpret initial)))
         (loop for position from first below limit
               do (setf value
                        (interpret-expression body
                                              (acons index position
                                                     (acons accumulator value variables))
                                              definitions)))
         value))
      ((pair-value _ left right) (list (interpret left) (interpret right)))
      ((projection index pair) (nth index (interpret pair)))
      ((invocation name arguments)
       (interpret-function (find-definition name definitions) (mapcar #'interpret arguments)
                           definitions)))))

(defun interpret-function (definition arguments definitions)
  (let* ((signature (checked-function-signature definition))
         (names (mapcar #'car (function-signature-parameters signature))))
    (interpret-expression (checked-function-body definition)
                          (mapcar #'cons names arguments)
                          definitions)))
