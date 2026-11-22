;;;; Read Tiny forms, collect function signatures, and check the bodies.

(defpackage #:zk-llvm/examples/compiler
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm))
  (:export #:main))

(in-package #:zk-llvm/examples/compiler)

(defstruct function-signature name parameters result)
(defstruct source-function signature body)
(defstruct checked-function signature body)

;; Nodes carry their inferred type where LLVM needs it to emit a value or PHI.
(zk:defvariant checked-expression
  (literal type value)
  (reference name)
  (binary operator left right)
  (binding name initializer body)
  (branch type test consequent alternative)
  (folding type index start end accumulator initial body)
  (pair-value type left right)
  (projection index pair)
  (invocation name arguments))

(defun call-with-tiny-reader (body)
  (let ((*readtable* (copy-readtable nil))
        (*package* (find-package '#:zk-llvm/examples/compiler)))
    ;; Reject reader labels so the type checker only needs to handle trees.
    (flet ((reject-label (stream character argument)
             (declare (ignore stream character argument))
             (error "Reader labels are not allowed in Tiny source.")))
      (set-dispatch-macro-character #\# #\= #'reject-label)
      (set-dispatch-macro-character #\# #\# #'reject-label))
    (funcall body)))

(defun read-forms (stream)
  (let ((*read-eval* nil) (eof (gensym)))
    (loop for form = (read stream nil eof)
          until (eq form eof)
          collect form)))

(defun read-program (pathname)
  (call-with-tiny-reader
   (lambda ()
     (with-open-file (stream pathname)
       (read-forms stream)))))

(defun variable-name-p (name)
  (and (symbolp name) (not (or (keywordp name) (member name '(nil t))))))

(defun check-type-form (form)
  (zk:match form
    ((or :i64 :i1) form)
    ((list 'pair left right)
     (check-type-form left)
     (check-type-form right)
     form)
    (_ (error "Invalid Tiny type ~S." form))))

(defun parse-parameters (parameters name)
  (unless (handler-case (list-length parameters) (type-error () nil))
    (error "Invalid parameter list in ~S." name))
  (let ((bindings nil))
    (dolist (parameter parameters (nreverse bindings))
      (zk:match parameter
        ((list variable type)
         (unless (variable-name-p variable)
           (error "Invalid parameter name ~S in ~S." variable name))
         (when (assoc variable bindings)
           (error "Duplicate parameter ~S in ~S." variable name))
         (push (cons variable (check-type-form type)) bindings))
        (_ (error "Invalid parameter ~S in ~S." parameter name))))))

(defun parse-function (form)
  (zk:match form
    ((list 'fn (type string name) parameters result body)
     (when (or (zerop (length name)) (find (code-char 0) name)
               (eql (search "llvm." name) 0))
       (error "Invalid function name ~S." name))
     (make-source-function
      :signature (make-function-signature :name name
                                          :parameters (parse-parameters parameters name)
                                          :result (check-type-form result))
      :body body))
    (_ (error "Invalid function definition ~S." form))))

(defun source-function-name (function)
  (function-signature-name (source-function-signature function)))

(defun checked-function-name (function)
  (function-signature-name (checked-function-signature function)))

(defun program-signatures (functions)
  (let ((signatures (make-hash-table :test 'equal)))
    (dolist (function functions signatures)
      (let ((name (source-function-name function)))
        (when (gethash name signatures) (error "Duplicate function ~S." name))
        (setf (gethash name signatures) (source-function-signature function))))))

(defun check-expression (form expected variables signatures)
  (multiple-value-bind (expression actual) (infer-expression form variables signatures)
    (unless (equal expected actual)
      (error "Expected ~S in ~S, got ~S." expected form actual))
    expression))

(defun infer-expression (form variables signatures)
  (flet ((infer (subform) (infer-expression subform variables signatures))
         (check (subform type) (check-expression subform type variables signatures)))
    (zk:match form
      ((type (signed-byte 64) value)
       (values (literal :i64 value) :i64))
      ((and (or :true :false) value)
       (values (literal :i1 (eq value :true)) :i1))
      ((type symbol name)
       (let ((type (cdr (assoc name variables))))
         (unless type (error "Unbound variable ~S." name))
         (values (reference name) type)))
      ((list (and (or '+ '- '* '<) operator) left right)
       (values (binary operator (check left :i64) (check right :i64))
               (if (eq operator '<) :i1 :i64)))
      ((list 'let (list (list name annotation initializer)) body)
       (unless (variable-name-p name) (error "Invalid binding name ~S." name))
       (let* ((type (check-type-form annotation))
              (initial (check initializer type)))
         (multiple-value-bind (checked-body body-type)
             (infer-expression body (acons name type variables) signatures)
           (values (binding name initial checked-body) body-type))))
      ((list 'if test consequent alternative)
       (let ((checked-test (check test :i1)))
         (multiple-value-bind (checked-consequent type) (infer consequent)
           (values (branch type checked-test checked-consequent (check alternative type))
                   type))))
      ((list 'fold (list index start end) (list accumulator initial) body)
       (unless (and (variable-name-p index) (variable-name-p accumulator)
                    (not (eq index accumulator)))
         (error "Invalid fold bindings ~S and ~S." index accumulator))
       (let ((checked-start (check start :i64)) (checked-end (check end :i64)))
         (multiple-value-bind (checked-initial type) (infer initial)
           (let* ((scope (acons index :i64 (acons accumulator type variables)))
                  (checked-body (check-expression body type scope signatures)))
             (values (folding type index checked-start checked-end accumulator
                              checked-initial checked-body)
                     type)))))
      ((list 'pair left right)
       (multiple-value-bind (checked-left left-type) (infer left)
         (multiple-value-bind (checked-right right-type) (infer right)
           (let ((type (list 'pair left-type right-type)))
             (values (pair-value type checked-left checked-right) type)))))
      ((list (and (or 'first 'second) accessor) operand)
       (multiple-value-bind (checked-operand type) (infer operand)
         (unless (consp type)
           (error "Expected a pair in ~S, got ~S." operand type))
         (let ((index (if (eq accessor 'first) 0 1)))
           (values (projection index checked-operand) (nth (1+ index) type)))))
      ((list* 'call (type string name) arguments)
       (let* ((callee (or (gethash name signatures) (error "Unknown function ~S." name)))
              (parameters (function-signature-parameters callee)))
         (unless (eql (list-length arguments) (length parameters))
           (error "~S expects ~D arguments." name (length parameters)))
         (values (invocation name
                             (loop for argument in arguments for parameter in parameters
                                   collect (check argument (cdr parameter))))
                 (function-signature-result callee))))
      (_ (error "Invalid expression ~S." form)))))

(defun check-functions (functions signatures)
  (loop for function in functions
        for signature = (source-function-signature function)
        collect (make-checked-function
                 :signature signature
                 :body (check-expression (source-function-body function)
                                         (function-signature-result signature)
                                         (function-signature-parameters signature)
                                         signatures))))

(defun check-program (forms)
  (unless forms (error "The source file has no definitions."))
  (let ((functions (mapcar #'parse-function forms)))
    (check-functions functions (program-signatures functions))))

(defun find-definition (name definitions)
  (or (find name definitions :test #'string= :key #'checked-function-name)
      (error "Unknown function ~S." name)))
