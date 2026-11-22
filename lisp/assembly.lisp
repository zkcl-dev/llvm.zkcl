(in-package #:zk-llvm)

(define-operation %make-inline-asm "make_inline_asm" :value
    (signature :handle) (assembly :pointer) (assembly-count :uintptr)
    (constraints :pointer) (constraints-count :uintptr) (options :uint32))
(define-operation inline-asm-signature "inline_asm_signature" :type (value :handle))
(define-operation %inline-asm-assembly "inline_asm_assembly" :bytes (value :handle))
(define-operation %inline-asm-constraints "inline_asm_constraints" :bytes (value :handle))
(define-operation %inline-asm-options "inline_asm_options" :uint64 (value :handle))
(define-operation block-address "block_address" :value (block :handle))
(define-operation block-address-target "block_address_target" :block (value :handle))
(define-operation %indirect-br "indirect_br" :instruction
    (builder :handle) (address :handle) (destinations :pointer) (count :uintptr))
(define-operation %call-br "call_br" :instruction
    (builder :handle) (assembly :handle) (arguments :pointer) (argument-count :uintptr)
    (normal :handle) (indirect :pointer) (indirect-count :uintptr)
    (bundles :pointer) (bundle-count :uintptr) (name :cstring))

(defun inline-asm (signature assembly constraints &key side-effects align-stack unwind (dialect :att))
  "Make a typed inline assembly callee. ASSEMBLY and CONSTRAINTS accept strings or octets.
Use BUILD-INDIRECT-CALL with the same SIGNATURE, or BUILD-CALL-BR for asm goto."
  (let ((signature (resolve-type signature))
        (options (logior (ecase dialect (:att 0) (:intel 1))
                         (cl:if side-effects 2 0) (cl:if align-stack 4 0)
                         (cl:if unwind 8 0))))
    (call-with-octets assembly
      (lambda (assembly-data assembly-count)
        (call-with-octets constraints
          (lambda (constraint-data constraint-count)
            (%make-inline-asm signature assembly-data assembly-count
                              constraint-data constraint-count options)))))))

(defun inline-asm-info (value)
  "Return the signature, template, constraints, and options of inline asm as a plist.
Return NIL for another value kind. Non-UTF-8 strings are returned as octets."
  (let ((signature (inline-asm-signature value)))
    (cl:when signature
      (let ((flags (%inline-asm-options value)))
        (list :signature signature
              :assembly (octets-or-text (%inline-asm-assembly value))
              :constraints (octets-or-text (%inline-asm-constraints value))
              :dialect (cl:if (logbitp 0 flags) :intel :att)
              :side-effects (logbitp 1 flags) :align-stack (logbitp 2 flags)
              :unwind (logbitp 3 flags))))))

(defun build-indirect-br (builder address destinations)
  "End the current block with an indirect branch. Supply every possible destination block."
  (call-with-handles destinations
    (lambda (data count) (%indirect-br builder address data count))))

(defun build-call-br (builder assembly arguments normal indirect
                      &key (bundles *operand-bundles*) (name ""))
  "Emit asm goto with NORMAL fallthrough and INDIRECT destination blocks in constraint order."
  (call-with-handles arguments
    (lambda (argument-data argument-count)
      (call-with-handles indirect
        (lambda (indirect-data indirect-count)
          (call-with-operand-bundles bundles
            (lambda (bundle-data bundle-count)
              (%call-br builder assembly argument-data argument-count normal
                        indirect-data indirect-count bundle-data bundle-count name))))))))

(defun indirect-br (address destinations)
  "Emit an indirect branch with the current builder and return its instruction."
  (build-indirect-br (ensure-builder) (ensure-value address :ptr) destinations))

(defun call-br (assembly normal indirect &rest arguments)
  "Emit asm goto with the current builder, coercing literal ARGUMENTS to its signature.
Return the instruction. Position the builder to emit its continuation blocks."
  (let ((signature (cl:or (inline-asm-signature assembly)
                          (error 'llvm-error :operation 'call-br
                                 :message "CALL-BR requires an inline assembly callee."))))
    (build-call-br (ensure-builder) assembly (coerce-call-arguments signature arguments 'call-br)
                   normal indirect)))
