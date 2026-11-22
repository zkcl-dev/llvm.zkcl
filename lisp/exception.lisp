(in-package #:zk-llvm)

(define-operation function-personality "function_personality" :value (function :handle))
(define-operation (%set-function-personality set-function-personality)
    "set_function_personality" :effect (function :handle) (personality :optional-handle))

(defun set-function-personality (function personality)
  "Attach a personality function or constant address to FUNCTION. NIL clears it."
  (%set-function-personality function (cl:when personality (ensure-value personality))))

(define-operation (%build-landing-pad build-landing-pad) "landing_pad" :value
    (builder :handle) (type :handle) (cleanup :bool) (clauses :pointer) (count :uintptr)
    (name :cstring))
(define-operation build-resume "resume" :effect (builder :handle) (exception :handle))
(define-operation %invoke-with-bundles "invoke_with_bundles" :instruction
    (builder :handle) (callee :handle) (arguments :pointer) (count :uintptr)
    (normal :handle) (unwind :handle) (bundles :pointer) (bundle-count :uintptr) (name :cstring))
(define-operation %indirect-invoke-with-bundles "invoke_indirect_with_bundles" :instruction
    (builder :handle) (signature :handle) (callee :handle) (arguments :pointer) (count :uintptr)
    (normal :handle) (unwind :handle) (bundles :pointer) (bundle-count :uintptr) (name :cstring))

(defun build-invoke (builder callee arguments normal unwind &rest options)
  "End the block with a direct invoke and return its instruction.
INSTRUCTION-RESULT returns the SSA value defined on the NORMAL edge, or NIL for void."
  (destructuring-bind (&key (name "") (bundles *operand-bundles*))
      (instruction-name-options options)
    (call-with-handles arguments
      (lambda (data count)
        (call-with-operand-bundles bundles
          (lambda (bundle-data bundle-count)
            (%invoke-with-bundles builder callee data count normal unwind
                                  bundle-data bundle-count name)))))))

(defun build-indirect-invoke (builder signature callee arguments normal unwind &rest options)
  "End the block with an indirect invoke using SIGNATURE or a prototype's ABI."
  (destructuring-bind (&key (name "") (bundles *operand-bundles*))
      (instruction-name-options options)
    (emit-indirect-call
     (lambda (builder signature callee data count name)
       (call-with-operand-bundles bundles
         (lambda (bundle-data bundle-count)
           (%indirect-invoke-with-bundles builder signature callee data count normal unwind
                                         bundle-data bundle-count name))))
     builder signature callee arguments name)))

(defun build-landing-pad (builder type &key cleanup clauses (name ""))
  "Emit an Itanium-style landingpad and return its exception aggregate.
TYPE matches the personality ABI, usually (:STRUCT :PTR :I32). Supply CLEANUP or
an ordered sequence of CLAUSES. Constant pointers catch, and constant pointer
arrays filter. A null pointer is a catch-all."
  (let ((type (resolve-type type (builder-context builder))))
    (call-with-handles (mapcar #'ensure-value (sequence->list clauses))
      (lambda (data count) (%build-landing-pad builder type cleanup data count name)))))

(defun invoke-call (callee normal unwind &rest arguments)
  "Invoke CALLEE in the current block, coercing literal arguments. Return the instruction.
Position the builder explicitly to emit the normal and unwind continuations."
  (let ((function (resolve-callee callee)))
    (build-invoke (ensure-builder) function
                  (coerce-call-arguments function arguments 'invoke-call)
                  normal unwind)))

(defun indirect-invoke (signature callee normal unwind &rest arguments)
  "Invoke CALLEE through a pointer with SIGNATURE or a prototype. Return the instruction."
  (emit-dsl-indirect-call
   'indirect-invoke
   (lambda (builder signature callee operands)
     (build-indirect-invoke builder signature callee operands normal unwind))
   signature callee arguments))

(defun landing-pad (type &key cleanup clauses (name ""))
  "Emit a landingpad with the current builder. See BUILD-LANDING-PAD."
  (build-landing-pad (ensure-builder) type :cleanup cleanup :clauses clauses :name name))

(defun resume (exception)
  "End the current block by resuming unwinding with EXCEPTION."
  (build-resume (ensure-builder) (ensure-value exception))
  (finish-ir-path))

(define-operation (%build-catch-switch build-catch-switch) "catch_switch" :instruction
    (builder :handle) (parent :optional-handle) (unwind :optional-handle) (name :cstring))
(define-operation (%build-catch-pad build-catch-pad) "catch_pad" :instruction
    (builder :handle) (parent :handle) (arguments :pointer) (count :uintptr) (name :cstring))
(define-operation (%build-cleanup-pad build-cleanup-pad) "cleanup_pad" :instruction
    (builder :handle) (parent :optional-handle) (arguments :pointer) (count :uintptr)
    (name :cstring))
(define-operation build-catch-ret "catch_ret" :instruction
    (builder :handle) (pad :handle) (destination :handle))
(define-operation (%build-cleanup-ret build-cleanup-ret) "cleanup_ret" :instruction
    (builder :handle) (pad :handle) (unwind :optional-handle))
(define-operation add-catch-switch-handler "add_catch_switch_handler" :effect
    (instruction :handle) (handler :handle))
(define-operation %catch-switch-handler-count "catch_switch_handler_count" :uint64
    (instruction :handle))
(define-operation %catch-switch-handler "catch_switch_handler" :block
    (instruction :handle) (index :uint32))
(define-operation exception-parent-pad "exception_parent_pad" :instruction
    (instruction :handle))
(define-operation exception-unwind-destination "exception_unwind_destination" :block
    (instruction :handle))

(defun build-catch-switch (builder &key parent unwind (name ""))
  "End the dispatch block with a catchswitch and return its instruction.
PARENT is an enclosing catchpad or cleanuppad instruction, or NIL at the outermost level.
Supply an UNWIND block or use NIL to unwind to the caller. Append handler blocks in
match order with ADD-CATCH-SWITCH-HANDLER before verifying the module."
  (%build-catch-switch builder parent unwind name))

(defun build-catch-pad (builder catch-switch arguments &optional (name ""))
  "Start a catch handler belonging to CATCH-SWITCH and return its instruction.
ARGUMENTS are typed values interpreted by the personality routine.
Use INSTRUCTION-RESULT to obtain the token for a funclet operand bundle."
  (call-with-handles (mapcar #'ensure-value (sequence->list arguments))
    (lambda (data count) (%build-catch-pad builder catch-switch data count name))))

(defun build-cleanup-pad (builder &key parent arguments (name ""))
  "Start a cleanup handler and return its instruction.
PARENT is an enclosing catchpad or cleanuppad instruction, or NIL at the outermost level.
ARGUMENTS are typed values interpreted by the personality routine.
Use INSTRUCTION-RESULT to obtain the token for a funclet operand bundle."
  (call-with-handles (mapcar #'ensure-value (sequence->list arguments))
    (lambda (data count) (%build-cleanup-pad builder parent data count name))))

(defun build-cleanup-ret (builder pad &optional unwind)
  "End the cleanup funclet entered by PAD. UNWIND defaults to exiting to the caller."
  (%build-cleanup-ret builder pad unwind))

(defun catch-switch-handlers (instruction)
  "Return the catchswitch's handler blocks in match order."
  (cl:loop for index below (%catch-switch-handler-count instruction)
           collect (%catch-switch-handler instruction index)))

(defun catch-switch (&rest options)
  "Emit a catchswitch with the current builder. See BUILD-CATCH-SWITCH."
  (apply #'build-catch-switch (ensure-builder) options))

(defun catch-pad (catch-switch arguments &optional (name ""))
  "Start a catch handler with the current builder and return its pad instruction."
  (build-catch-pad (ensure-builder) catch-switch arguments name))

(defun cleanup-pad (&rest options)
  "Start a cleanup handler with the current builder. See BUILD-CLEANUP-PAD."
  (apply #'build-cleanup-pad (ensure-builder) options))

(defun catch-ret (pad destination)
  "End the catch handler entered by PAD and continue at DESTINATION."
  (build-catch-ret (ensure-builder) pad destination))

(defun cleanup-ret (pad &optional unwind)
  "End the cleanup handler entered by PAD, unwinding to UNWIND or the caller."
  (build-cleanup-ret (ensure-builder) pad unwind))
