;;;; Submit checked definitions and evaluate expressions in an ORC session.

(in-package #:zk-llvm/examples/compiler)

(defstruct repl-expression name type body)
(defstruct repl-session jit functions)

(defun check-repl-expression (form signatures)
  (multiple-value-bind (body type) (infer-expression form nil signatures)
    (let ((name (loop for candidate = (symbol-name (gensym "tiny_repl_"))
                      unless (gethash candidate signatures) return candidate)))
      (make-repl-expression :name name :type type :body body))))

(defun result-word-count (type)
  (zk:match type
    ((list 'pair left right) (+ (result-word-count left) (result-word-count right)))
    (_ 1)))

(defun emit-repl-result (value type address)
  ;; The host calls a void function with an int64_t buffer. Store scalar
  ;; leaves from left to right, independent of how LLVM lays out a pair.
  (let ((index 0))
    (labels ((emit (value type)
               (zk:match type
                 ((list 'pair left right)
                  (emit (llvm:extract value 0) left)
                  (emit (llvm:extract value 1) right))
                 (_ (llvm:store (if (eq type :i1) (llvm:int-cast value :i64) value)
                                (llvm:gep-in-bounds :i64 address (list index)))
                    (incf index)))))
      (emit value type))))

(defun read-repl-result (type address word-size)
  (let ((offset 0))
    (labels ((read-value (type)
               (zk:match type
                 ((list 'pair left right) (list (read-value left) (read-value right)))
                 (_ (let ((word (zk-ffi:mem-ref address :int64 offset)))
                      (incf offset word-size)
                      (if (eq type :i1) (if (zerop word) :false :true) word))))))
      (read-value type))))

(defun compile-repl-expressions (jit expressions functions)
  (llvm:define-module (module "tiny-repl" :jit jit)
    (dolist (source functions)
      (declare-signature module (source-function-signature source)))
    (dolist (expression expressions)
      (let ((function (llvm:declare-function module (repl-expression-name expression) :void '(:ptr))))
        (llvm:emit-function-body
         function '("result")
         (lambda (parameters)
           (emit-repl-result (emit-expression (repl-expression-body expression) nil)
                             (repl-expression-type expression) (first parameters))
           (llvm:ret)))))))

(defun run-repl-expression (jit expression)
  (let* ((type (repl-expression-type expression))
         (word-size (zk-ffi:type-size :int64)))
    (zk-ffi:with-foreign-memory (address (* word-size (result-word-count type)))
      (call-jit jit (repl-expression-name expression) :void '(:ptr) (list address))
      (read-repl-result type address word-size))))

(defun run-repl-expressions (jit expressions functions)
  (when expressions
    (llvm:with-objects ()
      (let ((tracker (llvm:make-resource-tracker jit)))
        (unwind-protect
             (progn
               (llvm:jit-add-module jit (compile-repl-expressions jit expressions functions)
                                    :tracker tracker)
               (loop for expression in expressions collect (run-repl-expression jit expression)))
          (llvm:remove-resource-tracker tracker))))))

(defun install-repl-submission (jit definitions imports expressions functions)
  (if (null definitions)
      (run-repl-expressions jit expressions functions)
      (llvm:with-objects ()
        (let ((tracker (llvm:make-resource-tracker jit))
              (installed nil))
          (unwind-protect
               (progn
                 (llvm:jit-add-module jit (compile-program definitions :imports imports :jit jit)
                                      :tracker tracker)
                 ;; ORC links on lookup. Resolve every new name before publishing it.
                 (dolist (definition definitions)
                   (llvm:release (llvm:jit-lookup jit (checked-function-name definition))))
                 (multiple-value-prog1 (run-repl-expressions jit expressions functions)
                   (setf installed t)))
            ;; On success, dropping the tracker keeps the definitions in the JIT.
            (unless installed (llvm:remove-resource-tracker tracker)))))))

(defun dispose-repl-jit (jit)
  (unwind-protect (llvm:close-jit jit)
    (llvm:release jit)))

(defun merge-repl-functions (current updates)
  (let* ((signatures (program-signatures updates))
         (remaining (remove-if (lambda (function)
                                 (gethash (source-function-name function) signatures))
                               current)))
    (dolist (function remaining)
      (setf (gethash (source-function-name function) signatures)
            (source-function-signature function)))
    (values (append remaining updates) signatures (/= (length remaining) (length current)))))

(defun submit-repl-forms (session forms output)
  (multiple-value-bind (updates forms-to-run)
      (loop for form in forms
            if (and (consp form) (eq (car form) 'fn))
              collect (parse-function form) into definitions
            else collect form into expressions
            finally (return (values definitions expressions)))
    (let ((current (repl-session-functions session)))
      (multiple-value-bind (functions signatures replacing) (merge-repl-functions current updates)
        (let* ((definitions (check-functions (if replacing functions updates) signatures))
               (expressions (mapcar (lambda (form) (check-repl-expression form signatures))
                                    forms-to-run))
               (previous (repl-session-jit session))
               (replacement (when replacing (llvm:make-jit)))
               (results
                 (unwind-protect
                      (multiple-value-prog1
                          (install-repl-submission (or replacement previous) definitions
                                                   (unless replacing current) expressions functions)
                        (setf (repl-session-functions session) functions)
                        (when replacing
                          ;; Existing native calls use resolved addresses. Rebuild their
                          ;; definitions in a fresh JIT before dropping the old code.
                          (setf (repl-session-jit session) replacement
                                replacement nil)
                          (dispose-repl-jit previous)))
                   (when replacement (dispose-repl-jit replacement)))))
          (dolist (function updates)
            (format output "Defined ~S~%" (source-function-name function)))
          (dolist (result results) (format output "~S~%" result)))))))

(defun read-repl-submission (source)
  (handler-case
      (values (call-with-tiny-reader
               (lambda ()
                 (with-input-from-string (stream source)
                   (read-forms stream))))
              t)
    (end-of-file () (values nil nil))))

(defun run-repl (&optional source)
  (let ((interactive (interactive-stream-p *standard-input*))
        (session (make-repl-session :jit (llvm:make-jit)))
        (pending nil))
    (unwind-protect
         (progn
           (when source (submit-repl-forms session (read-program source) *standard-output*))
           (loop
             (when interactive
               (write-string (if pending "....> " "tiny> "))
               (finish-output))
             (let ((line (read-line *standard-input* nil nil)))
               (unless line
                 (when interactive (terpri))
                 (when pending (format *error-output* "Tiny: Unexpected end of input.~%"))
                 (return))
               (setf pending (if pending (concatenate 'string pending (string #\Newline) line) line))
               (handler-case
                   (multiple-value-bind (forms complete) (read-repl-submission pending)
                     (when complete
                       (setf pending nil)
                       (when forms (submit-repl-forms session forms *standard-output*))))
                 (error (condition)
                   (setf pending nil)
                   (format *error-output* "Tiny: ~A~%" condition))))))
      (dispose-repl-jit (repl-session-jit session)))))
