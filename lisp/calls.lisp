(in-package #:zk-llvm)

(defvar *operand-bundles* nil
  "Default operand bundles for calls emitted in the current dynamic scope.")

(defmacro with-operand-bundles ((&rest bundles) &body body)
  "Evaluate BUNDLES once and use them for calls emitted by BODY.
For a funclet, use (OPERAND-BUNDLE \"funclet\" (INSTRUCTION-RESULT pad)).
Explicit :BUNDLES on a builder call overrides the scope's defaults."
  `(let ((*operand-bundles* (list ,@bundles)))
     ,@body))

(define-operation %make-operand-bundle "make_operand_bundle" :operand-bundle
    (tag :pointer) (tag-length :uintptr) (operands :pointer) (count :uintptr))
(define-operation (%operand-bundle-tag operand-bundle-tag) "operand_bundle_tag" :bytes
    (bundle :handle))
(define-operation %operand-bundle-operand-count "operand_bundle_operand_count" :uint64
    (bundle :handle))
(define-operation %operand-bundle-operand "operand_bundle_operand" :value
    (bundle :handle) (index :uintptr))
(define-operation %call-operand-bundle-count "call_operand_bundle_count" :uint64
    (instruction :handle))
(define-operation %call-operand-bundle "call_operand_bundle" :operand-bundle
    (instruction :handle) (index :uint32))

(defun operand-bundle (tag &rest operands)
  "Make an LLVM call-site bundle with TAG and typed OPERANDS, in order."
  (call-with-octets tag
    (lambda (tag-data tag-count)
      (call-with-handles (mapcar #'ensure-value operands)
        (lambda (data count) (%make-operand-bundle tag-data tag-count data count))))))

(defun operand-bundle-tag (bundle)
  "Return the bundle's UTF-8 tag, or its octets when the tag is not UTF-8."
  (octets-or-text (%operand-bundle-tag bundle)))

(defun operand-bundle-operands (bundle)
  "Return the bundle's typed operands in order."
  (cl:loop for index below (%operand-bundle-operand-count bundle)
           collect (%operand-bundle-operand bundle index)))

(defmethod print-object ((object operand-bundle) stream)
  (print-unreadable-object (object stream :type t)
    (cl:if (handle-alive-p object)
           (prin1 (operand-bundle-tag object) stream)
           (write-string ":RELEASED" stream))))

(defun call-operand-bundles (instruction)
  "Return call-site bundles and an applicability flag for call, invoke, or callbr."
  (let ((count (%call-operand-bundle-count instruction)))
    (cl:if (eql count #xffffffffffffffff)
           (values nil nil)
           (values (cl:loop for index below count
                            collect (%call-operand-bundle instruction index))
                   t))))

(defun call-with-operand-bundles (bundles function)
  (call-with-handles
   (mapcar (lambda (bundle)
             (cl:if (operand-bundle-p bundle) bundle
                    (apply #'operand-bundle (sequence->list bundle))))
           (sequence->list bundles))
   function))

(define-operation %call-with-bundles "call_instruction_with_bundles" :instruction
    (builder :handle) (callee :handle) (arguments :pointer) (count :uintptr)
    (bundles :pointer) (bundle-count :uintptr) (name :cstring))
(define-operation %indirect-call-with-bundles "call_indirect_instruction_with_bundles" :instruction
    (builder :handle) (signature :handle) (callee :handle) (arguments :pointer) (count :uintptr)
    (bundles :pointer) (bundle-count :uintptr) (name :cstring))

(defun build-call-instruction (builder callee arguments &rest options)
  "Emit a direct call. Accept a positional name, or :NAME and :BUNDLES options."
  (destructuring-bind (&key (name "") (bundles *operand-bundles*))
      (instruction-name-options options)
    (call-with-handles arguments
      (lambda (data count)
        (call-with-operand-bundles bundles
          (lambda (bundle-data bundle-count)
            (%call-with-bundles builder callee data count bundle-data bundle-count name)))))))

(defun build-call (builder callee arguments &rest options)
  "Emit a direct call and return its SSA result, or NIL for a void call."
  (instruction-result (apply #'build-call-instruction builder callee arguments options)))

(defun build-indirect-call-instruction (builder signature callee arguments &rest options)
  "Emit an indirect call using a function type or prototype, with :NAME and :BUNDLES."
  (destructuring-bind (&key (name "") (bundles *operand-bundles*))
      (instruction-name-options options)
    (emit-indirect-call
     (lambda (builder signature callee data count name)
       (call-with-operand-bundles bundles
         (lambda (bundle-data bundle-count)
           (%indirect-call-with-bundles builder signature callee data count
                                        bundle-data bundle-count name))))
     builder signature callee arguments name)))

(defun build-indirect-call (builder signature callee arguments &rest options)
  "Emit an indirect call and return its SSA result, or NIL for a void call."
  (instruction-result
   (apply #'build-indirect-call-instruction builder signature callee arguments options)))

(defun resolve-callee (callee &optional (module *module*))
  (cl:cond
    ((function-p callee) callee)
    ((cl:or (stringp callee) (symbolp callee))
     (let ((name (c-symbol-name callee)))
       (cl:or (find-function (ensure-module module) name)
              (error "Unknown LLVM function ~S in active module." name))))
    (t (error "Expected an LLVM function handle or name, got ~S." callee))))

(defun coerce-call-arguments (signature-or-prototype arguments operation)
  ;; Typed operands can go straight to the builder's signature validation.
  ;; Only literals need type queries. Subtype zero is the return type.
  (let (signature)
    (cl:loop for argument in arguments
             for index from 1
             collect
             (cl:or (operand-value argument)
                    (progn
                      (cl:unless signature
                        (setf signature (cl:if (function-p signature-or-prototype)
                                               (function-signature signature-or-prototype)
                                               signature-or-prototype)))
                      (let ((type (type-subtype signature index)))
                        (cl:cond
                          (type (const type argument))
                          ((function-type-variadic-p signature)
                           (error 'llvm-error :operation operation
                                  :message (format nil "Variadic argument ~D needs a type. Use (ZK-LLVM:CONST TYPE VALUE)."
                                                   index)))
                          (t (error 'llvm-error :operation operation
                                    :message (format nil "Expected ~D arguments, got ~D."
                                                     (1- (type-subtype-count signature))
                                                     (length arguments)))))))))))

(defun call (callee &rest arguments)
  "Call a function handle or name in *MODULE*. Fixed parameters supply literal types.
Supply typed LLVM values for variadic arguments. A void call returns zero values."
  (let ((function (resolve-callee callee)))
    (cl:or (build-call (ensure-builder) function
                       (coerce-call-arguments function arguments 'call))
           (values))))

(defun call-instruction (callee &rest arguments)
  "Emit a direct call with the argument rules of CALL and return its instruction."
  (let ((function (resolve-callee callee)))
    (build-call-instruction (ensure-builder) function
                            (coerce-call-arguments function arguments 'call-instruction))))

(defun emit-dsl-indirect-call (operation emit signature callee arguments)
  (let* ((builder (ensure-builder))
         (signature (cl:if (handle-p signature)
                            signature
                            (resolve-type signature (builder-context builder))))
         (coerced (coerce-call-arguments signature arguments operation)))
    (funcall emit builder signature (ensure-value callee :ptr) coerced)))

(defun indirect-call (signature callee &rest arguments)
  "Call CALLEE through a pointer using a function type spec or prototype function.
Fixed parameters supply literal types. Variadic arguments require typed LLVM values."
  (cl:or (emit-dsl-indirect-call 'indirect-call #'build-indirect-call signature callee arguments)
         (values)))

(defun indirect-call-instruction (signature callee &rest arguments)
  "Emit an indirect call with the argument rules of INDIRECT-CALL and return its instruction."
  (emit-dsl-indirect-call 'indirect-call-instruction #'build-indirect-call-instruction
                          signature callee arguments))
