(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ZK-FFI")
    (error "zk-llvm requires a zkcl runtime with native FFI enabled. Use a zkcl CLI built with the ffi feature.")))

(defpackage #:zk-llvm
  (:use #:cl)
  (:local-nicknames (#:tl #:zk-treelist)
                    (#:ffi #:zk-ffi))
  (:shadow
   #:+ #:- #:* #:/ #:rem
   #:and #:or #:not
   #:= #:/= #:< #:<= #:> #:>=
   #:if #:when #:unless #:cond
   #:load #:loop #:break #:continue)
  (:export
   ;; Conditions & ownership
   #:llvm-error #:llvm-error-operation #:llvm-error-message
   #:handle #:handle-p #:handle-alive-p #:release
   #:with-objects #:with-llvm-objects
   ;; Strongly-typed handle classes & predicates
   #:context #:context-p
   #:module #:module-p
   #:module-edit #:module-edit-p
   #:builder #:builder-p #:builder-block
   #:ir-type #:type-p
   #:value #:value-p
   #:ir-function #:function-p
   #:ir-block #:block-p #:block-terminated-p
   #:instruction #:instruction-p
   #:operand-bundle #:operand-bundle-p #:operand-bundle-tag #:operand-bundle-operands
   #:global #:global-p #:global-alias #:global-alias-p
   #:target-machine #:target-machine-p #:data-layout #:data-layout-p
   #:metadata #:metadata-p #:comdat #:comdat-p
   #:debug-info #:debug-info-p
   #:debug-file #:debug-file-p
   #:debug-scope #:debug-scope-p
   #:debug-location #:debug-location-p
   #:debug-type #:debug-type-p #:debug-member #:debug-member-p
   #:debug-variable #:debug-variable-p
   #:jit #:jit-p
   #:jit-symbol #:jit-symbol-p
   #:resource-tracker #:resource-tracker-p
   ;; Implicit context & high-level DSL
   #:*context* #:*module* #:*function* #:*builder*
   #:*operand-bundles* #:with-operand-bundles
   #:resolve-type #:type-spec #:const #:ensure-value
   #:with-context #:with-module #:define-module #:with-builder
   #:defir #:declare-ir #:defglobal
   ;; High-level polymorphic IR operators (used via `llvm:+`, `llvm:if`, etc.)
   #:+ #:- #:* #:/ #:udiv #:sdiv #:fdiv
   #:rem #:urem #:srem #:frem
   #:and #:or #:xor #:not #:shl #:lshr #:ashr
   #:= #:/= #:< #:<= #:> #:>= #:cmp #:float-binop #:fneg
   #:cast #:int-cast #:select
   #:alloca #:load #:store #:gep #:gep-in-bounds #:extract #:insert
   #:int-binop #:fence #:atomic-rmw #:cmpxchg #:memcpy #:memmove #:memset
   #:malloc #:free #:ptr-diff
   #:extract-element #:insert-element #:shuffle-vector #:freeze
   #:call #:indirect-call #:call-instruction #:indirect-call-instruction #:ret #:unreachable
   #:if #:when #:unless #:cond
   #:while #:loop #:recur #:break #:continue #:switch
   ;; Context, module & types
   #:make-context #:make-module #:make-builder #:parse-ir #:parse-bitcode
   #:read-module #:write-module
   #:module-context #:module-ir #:module-bitcode #:verify-module #:link-modules
   #:copy-module #:with-module-copy
   #:module-identifier #:module-set-identifier #:module-source-filename #:module-set-source-filename
   #:module-inline-asm #:module-set-inline-asm #:module-append-inline-asm
   #:int-type #:float-type #:double-type #:void-type #:pointer-type
   #:half-type #:bfloat-type #:x86-fp80-type #:fp128-type #:ppc-fp128-type
   #:label-type #:metadata-type #:token-type #:type-sized-p #:type-floating-point-p
   #:floating-point-width #:type-subtype-count #:scalar-type
   #:array-type #:vector-type #:struct-type #:opaque-struct-type #:find-struct-type #:set-struct-body
   #:function-type #:type-ir #:type-kind #:type-equal-p #:type-subtype #:type-subtypes
   #:integer-width #:pointer-address-space #:array-length #:vector-length #:struct-name #:struct-opaque-p
   #:struct-packed-p #:function-type-variadic-p #:value-type #:value-ir #:value-equal-p #:value-constant-p
   #:value-name #:set-value-name
   ;; Constants & globals
   #:const-int #:constant-integer #:const-float #:const-float-from-string #:const-zero #:const-undef #:const-poison
   #:const-bytes #:const-array #:const-struct #:const-vector #:const-splat #:const-gep
   #:add-global #:find-global #:module-globals #:global-name #:global-storage-type #:global-address
   #:global-constant-p #:global-initializer #:set-global-initializer
   #:global-alignment #:set-global-alignment #:global-linkage #:set-global-linkage
   #:set-global-name #:global-equal-p
   #:global-unnamed-address #:set-global-unnamed-address
   #:add-global-string #:global-declaration-p #:global-address-space
   #:set-global-constant #:clear-global-initializer
   #:global-externally-initialized-p #:set-global-externally-initialized
   #:global-thread-local-mode #:set-global-thread-local-mode
   #:global-visibility #:set-global-visibility #:global-dll-storage-class #:set-global-dll-storage-class
   #:global-section #:set-global-section #:global-comdat #:set-global-comdat
   #:get-or-insert-comdat #:comdat-selection-kind #:set-comdat-selection-kind
   #:add-global-alias #:find-global-alias #:module-global-aliases #:global-alias-equal-p
   #:global-alias-module #:global-alias-name #:set-global-alias-name #:global-alias-value-type
   #:global-alias-address #:global-alias-address-space #:global-alias-target #:set-global-alias-target
   #:global-alias-linkage
   ;; Functions & blocks
   #:declare-function #:define-function #:emit-function-body
   #:add-function #:find-function #:declare-intrinsic #:module-functions
   #:function-name #:set-function-name #:function-equal-p #:function-module #:function-signature #:function-address
   #:function-linkage #:set-function-linkage
   #:function-unnamed-address #:set-function-unnamed-address
   #:function-calling-convention #:set-function-calling-convention #:set-function-attribute
   #:function-declaration-p #:function-visibility #:set-function-visibility
   #:function-dll-storage-class #:set-function-dll-storage-class
   #:function-section #:set-function-section #:function-comdat #:set-function-comdat
   #:function-attribute #:function-attributes #:function-has-attribute-p #:remove-function-attribute
   #:parameter #:function-parameters #:function-parameter-types #:function-return-type
   #:append-block #:insert-block-before #:function-blocks #:find-block #:block-name #:set-block-name
   #:block-function #:block-next #:block-previous #:block-equal-p
   #:block-successors #:block-predecessors #:function-reverse-postorder
   #:block-address #:block-address-target
   #:value-argument-function #:value-argument-index
   #:position-at-end #:position-before #:clear-position
   ;; Instruction inspection and editing
   #:block-instructions #:find-instruction #:block-terminator #:instruction-opcode #:instruction-ir #:instruction-block
   #:block-first-instruction #:block-last-instruction #:instruction-next #:instruction-previous
   #:instruction-equal-p #:instruction-name #:set-instruction-name
   #:instruction-icmp-predicate #:instruction-fcmp-predicate
   #:instruction-icmp-same-sign-p #:instruction-volatile-p
   #:set-instruction-icmp-same-sign
   #:instruction-int-flags #:set-instruction-int-flags #:gep-flags #:set-gep-flags
   #:instruction-fast-math-flags #:add-instruction-fast-math-flags
   #:edit-set-fast-math-flags
   #:instruction-result #:instruction-operand #:instruction-operands
   #:value-kind #:value-instruction #:value-block #:value-function #:value-global #:value-global-alias
   #:value-operand #:value-operands #:value-uses
   #:call-target #:call-signature #:call-arguments #:call-operand-bundles
   #:gep-pointer #:gep-source-type #:gep-index-operands #:gep-in-bounds-p
   #:alloca-allocated-type #:alloca-element-count #:load-pointer #:store-pointer #:stored-value
   #:instruction-successors #:phi-incomings #:replace-instruction-operand
   #:instruction-alignment #:set-instruction-alignment
   #:instruction-memory-ordering #:set-instruction-memory-ordering
   #:instruction-atomic-sync-scope #:set-instruction-atomic-sync-scope
   #:instruction-cmpxchg-success-ordering #:instruction-cmpxchg-failure-ordering
   #:set-instruction-cmpxchg-orderings #:instruction-cmpxchg-weak-p #:set-instruction-cmpxchg-weak
   #:instruction-atomic-rmw-operation #:set-instruction-atomic-rmw-operation #:set-instruction-volatile
   #:begin-edit #:finish-edit #:with-module-edit
   #:draft-instruction #:draft-value #:with-edit-builder
   #:edit-replace-operand #:edit-replace-all-uses #:edit-move-before #:edit-move-after
   #:edit-erase-instruction #:edit-redirect-successor
   #:switch-cases #:edit-add-switch-case #:edit-remove-switch-case
   #:edit-detach-instruction #:edit-insert-instruction-before #:edit-insert-instruction-after
   #:edit-move-block-before #:edit-move-block-after #:edit-erase-block
   #:edit-detach-block #:edit-insert-block-before #:edit-insert-block-after
   #:tail-call-kind #:set-tail-call-kind #:call-calling-convention #:set-call-calling-convention
   #:set-call-attribute #:call-attribute #:call-attributes #:call-has-attribute-p #:remove-call-attribute
   ;; Low-level builder instructions
   #:build-add #:build-sub #:build-mul #:build-int-binop #:build-float-binop #:build-fneg
   #:build-icmp #:build-fcmp #:build-int-cast #:build-cast
   #:build-phi #:add-incoming #:build-br #:build-cond-br #:build-ret #:build-call
   #:build-select #:build-switch #:build-unreachable #:build-indirect-call
   #:build-call-instruction #:build-indirect-call-instruction
   #:inline-asm #:inline-asm-signature #:inline-asm-info #:build-call-br #:build-indirect-br
   #:call-br #:indirect-br
   #:float-binop #:fneg #:fdiv #:frem
   #:build-alloca #:build-load #:build-store #:build-gep #:build-gep-in-bounds
   #:build-extract-value #:build-insert-value
   #:build-extract-element #:build-insert-element #:build-shuffle-vector #:build-freeze #:build-not
   #:build-fence #:build-atomic-rmw #:build-cmpxchg #:build-memcpy #:build-memmove #:build-memset
   #:build-malloc #:build-free #:build-ptr-diff
   #:function-personality #:set-function-personality
   #:build-invoke #:build-indirect-invoke #:build-landing-pad #:build-resume
   #:invoke-call #:indirect-invoke #:landing-pad #:resume
   #:build-catch-switch #:build-catch-pad #:build-cleanup-pad #:build-catch-ret #:build-cleanup-ret
   #:catch-switch #:catch-pad #:cleanup-pad #:catch-ret #:cleanup-ret
   #:add-catch-switch-handler #:catch-switch-handlers #:exception-parent-pad #:exception-unwind-destination
   ;; Target & optimization
   #:host-triple #:host-cpu #:host-features #:make-target-machine #:target-triple #:target-cpu #:target-features #:target-data-layout
   #:target-abi #:target-backend-name #:target-backend-description
   #:module-target-triple #:set-module-target-triple #:module-data-layout #:set-module-layout #:configure-module-target
   #:make-data-layout #:target-layout #:module-layout #:layout-text #:layout-little-endian-p
   #:layout-pointer-size-bits #:layout-pointer-index-size-bits #:layout-non-integral-pointer-p
   #:layout-size-in-bits #:layout-store-size #:layout-alloc-size
   #:layout-abi-alignment #:layout-preferred-alignment #:layout-field-offset
   #:target-abi-size #:target-abi-alignment #:target-field-offset
   #:optimize-module #:optimize-function #:emit-object #:emit-assembly
   #:pass-options #:make-pass-options #:pass-options-p
   #:pass-options-verify-each #:pass-options-debug-logging #:pass-options-aa-pipeline
   #:pass-options-loop-interleaving #:pass-options-loop-vectorization
   #:pass-options-slp-vectorization #:pass-options-loop-unrolling
   #:pass-options-forget-all-scev-in-loop-unroll #:pass-options-call-graph-profile
   #:pass-options-merge-functions #:pass-options-licm-mssa-opt-cap
   #:pass-options-licm-mssa-no-acc-for-promotion-cap #:pass-options-inliner-threshold
   ;; Debug metadata
   #:make-debug-info #:with-debug-info #:make-debug-file #:attach-function-debug-info
   #:make-debug-lexical-block #:make-debug-location #:set-debug-location #:with-debug-location #:finish-debug-info
   #:function-debug-scope #:debug-scope-file #:debug-scope-line
   #:builder-debug-location #:finish-debug-function
   #:make-debug-type #:make-debug-variable #:record-debug-value
   #:make-debug-pointer-type #:make-debug-array-type #:make-debug-member
   #:make-debug-struct-type #:make-debug-union-type #:record-debug-address
   #:make-debug-inlined-location #:debug-location-line #:debug-location-column
   #:debug-location-file #:debug-location-inlined-at #:debug-file-filename #:debug-file-directory
   #:instruction-debug-location #:set-instruction-debug-location
   #:metadata-string #:metadata-node #:value-metadata #:metadata-ir
   #:metadata-string-value #:metadata-integer-value
   #:metadata-operand-count #:metadata-operand #:metadata-operands
   #:module-add-named-metadata #:module-named-metadata-count #:module-named-metadata-operand #:module-named-metadata
   #:module-flag #:module-add-flag #:module-strip-debug-info
   ;; ORC JIT
   #:make-jit #:make-jit-dylib #:jit-add-module #:jit-add-ir-module #:jit-add-object #:jit-define-symbol
   #:make-resource-tracker #:remove-resource-tracker
   #:jit-triple #:jit-data-layout #:jit-configure-module #:jit-lookup
   #:bind-jit-function #:jit-symbol-address #:close-jit
   #:with-jit #:with-jit-module #:with-jit-binding #:jit-bind #:jit-fn))
