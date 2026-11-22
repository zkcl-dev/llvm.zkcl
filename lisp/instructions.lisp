(in-package #:zk-llvm)

(define-operation block-first-instruction "block_first_instruction" :instruction (block :handle))
(define-operation block-terminator "block_terminator" :instruction (block :handle))
(define-operation instruction-next "instruction_next" :instruction (instruction :handle))
(define-operation (%instruction-opcode instruction-opcode) "instruction_opcode" :text
    (instruction :handle))
(define-operation instruction-ir "instruction_ir" :text (instruction :handle))
(define-operation instruction-block "instruction_block" :block (instruction :handle))
(define-operation instruction-result "instruction_result" :value (instruction :handle))
(define-operation %instruction-operand-count "instruction_operand_count" :uint64
    (instruction :handle))
(define-operation instruction-operand "instruction_operand" :value
    (instruction :handle) (index :uint32))
(define-operation %instruction-successor-count "instruction_successor_count" :uint64
    (instruction :handle))
(define-operation %instruction-successor "instruction_successor" :block
    (instruction :handle) (index :uint32))
(define-operation %phi-incoming-block "instruction_phi_incoming_block" :block
    (instruction :handle) (index :uint32))
(define-operation (%replace-instruction-operand replace-instruction-operand)
    "instruction_replace_operand" :effect (instruction :handle) (index :uint32) (value :handle))
(define-operation block-equal-p "block_eq" :bool (left :handle) (right :handle))
(define-operation value-equal-p "value_eq" :bool (left :handle) (right :handle))
(define-operation instruction-alignment "instruction_alignment" :uint64 (instruction :handle))
(define-operation set-instruction-alignment "set_instruction_alignment" :effect
    (instruction :handle) (alignment :uint64))
(define-operation (%instruction-fast-math-flags instruction-fast-math-flags)
    "instruction_fast_math_flags" :uint64 (instruction :handle))
(define-operation (%add-instruction-fast-math-flags add-instruction-fast-math-flags)
    "add_instruction_fast_math_flags" :effect (instruction :handle) (flags :uint32))
(define-operation (%instruction-int-flags instruction-int-flags)
    "instruction_int_flags" :uint64 (instruction :handle))
(define-operation (%set-instruction-int-flags set-instruction-int-flags)
    "set_instruction_int_flags" :effect (instruction :handle) (flags :uint32))

(define-operation (%value-kind value-kind) "value_kind" :text (value :handle))
(define-operation value-instruction "value_instruction" :instruction (value :handle))
(define-operation value-block "value_block" :block (value :handle))
(define-operation value-function "value_function" :function (value :handle))
(define-operation value-global "value_global" :global (value :handle))
(define-operation value-global-alias "value_global_alias" :global-alias (value :handle))
(define-operation %value-operand-count "value_operand_count" :uint64 (value :handle))
(define-operation value-operand "value_operand" :value (value :handle) (index :uint32))
(define-operation (%value-uses value-uses) "value_uses" :pointer
    (value :handle) (module :optional-handle))
(define-operation %value-use-count "value_use_count" :uint64 (uses :pointer))
(define-operation %value-use-user "value_use_user" :value
    (uses :pointer) (index :uintptr))
(define-operation %value-use-operand-index "value_use_operand_index" :uint64
    (uses :pointer) (index :uintptr))

(define-operation call-target "call_target" :value (instruction :handle))
(define-operation call-signature "call_signature" :type (instruction :handle))
(define-operation %call-argument-count "call_argument_count" :uint64 (instruction :handle))
(define-operation %call-argument "call_argument" :value (instruction :handle) (index :uint32))
(define-operation gep-pointer "gep_pointer" :value (instruction :handle))
(define-operation gep-source-type "gep_source_type" :type (instruction :handle))
(define-operation %gep-index-count "gep_index_count" :uint64 (instruction :handle))
(define-operation %gep-index "gep_index" :value (instruction :handle) (index :uint32))
(define-operation gep-in-bounds-p "gep_is_in_bounds" :bool (instruction :handle))
(define-operation (%gep-flags gep-flags) "instruction_gep_flags" :uint64 (instruction :handle))
(define-operation (%set-gep-flags set-gep-flags) "set_instruction_gep_flags" :effect
    (instruction :handle) (flags :uint32))
(define-operation load-pointer "load_pointer" :value (instruction :handle))
(define-operation store-pointer "store_pointer" :value (instruction :handle))
(define-operation stored-value "stored_value" :value (instruction :handle))
(define-operation alloca-allocated-type "alloca_allocated_type" :type (instruction :handle))
(define-operation alloca-element-count "alloca_element_count" :value (instruction :handle))
(define-operation instruction-volatile-p "instruction_is_volatile" :bool (instruction :handle))
(define-operation instruction-icmp-same-sign-p "instruction_icmp_is_same_sign" :bool
    (instruction :handle))
(define-operation set-instruction-icmp-same-sign "set_instruction_icmp_same_sign" :effect
    (instruction :handle) (same-sign :bool))

(defun value-kind (value)
  "Return the LLVM value kind as a keyword, for example :ARGUMENT or :CONSTANT-INT.
Constant kinds without a dedicated LLVM C API tag yield :OPAQUE-CONSTANT."
  (intern (string-upcase (%value-kind value)) :keyword))

(defun value-operands (value)
  "Return VALUE's LLVM User operands in index order, with NIL for empty slots."
  (cl:loop for index below (%value-operand-count value) collect (value-operand value index)))

(defun value-uses (value &optional module)
  "Return direct uses in MODULE as (USER-VALUE OPERAND-INDEX) pairs.
The index is zero-based. Duplicate uses of VALUE in the same user have separate
entries. Constant users are included when reachable from the module's IR.
MODULE defaults to the value's owning module when it has one. Pass MODULE
explicitly for a context-owned constant. Metadata references and elements
encoded inside constant data arrays and vectors have no value operand slots."
  (let ((uses (%value-uses value module)))
    (unwind-protect
        (cl:loop for index below (%value-use-count uses)
                 collect (list (%value-use-user uses index)
                               (%value-use-operand-index uses index)))
      (%release uses))))

(defun call-arguments (instruction)
  "Return actual arguments in order, excluding operand bundles and the callee.
The second value is true for CALL, INVOKE, or CALLBR."
  (let ((count (%call-argument-count instruction)))
    (cl:if (eql count #xffffffffffffffff)
           (values nil nil)
           (values (cl:loop for index below count collect (%call-argument instruction index))
                   t))))

(defun gep-index-operands (instruction)
  "Return GEP index values in order, excluding the base pointer.
The second value is true for a GEP instruction."
  (let ((count (%gep-index-count instruction)))
    (cl:if (eql count #xffffffffffffffff)
           (values nil nil)
           (values (cl:loop for index below count collect (%gep-index instruction index))
                   t))))

(defparameter *gep-flag-names* '(:in-bounds :nusw :nuw))
(defparameter *instruction-int-flag-names* '(:nsw :nuw :exact))

(defun instruction-flags-from-mask (mask names)
  (cl:if (eql mask #xffffffffffffffff)
         (values nil nil)
         (values (cl:loop for flag in names for index from 0
                          when (logbitp index mask) collect flag)
                 t)))

(defun instruction-flags-mask (operation flags names)
  (let ((mask 0))
    (dolist (flag (cl:if (keywordp flags) (list flags) flags) mask)
      (setf mask (logior mask (ash 1 (operation-index operation flag names)))))))

(defun gep-flags (instruction)
  "Return the no-wrap flag list and whether INSTRUCTION is a GEP.
:IN-BOUNDS implies :NUSW, so both appear when :IN-BOUNDS is set."
  (instruction-flags-from-mask (%gep-flags instruction) *gep-flag-names*))

(defun set-gep-flags (instruction flags)
  "Replace all GEP no-wrap flags with FLAGS, a flag keyword or list.
Pass NIL to clear all flags. :IN-BOUNDS implies :NUSW."
  (%set-gep-flags instruction (instruction-flags-mask 'set-gep-flags flags *gep-flag-names*)))

(defun instruction-int-flags (instruction)
  "Return the :NSW, :NUW, and :EXACT flags as a list and whether INSTRUCTION supports them."
  (instruction-flags-from-mask (%instruction-int-flags instruction) *instruction-int-flag-names*))

(defun set-instruction-int-flags (instruction flags)
  "Replace all integer no-wrap or exact flags with FLAGS, a flag keyword or list.
Pass NIL to clear all flags. The opcode determines which flags are allowed."
  (%set-instruction-int-flags
   instruction (instruction-flags-mask 'set-instruction-int-flags flags *instruction-int-flag-names*)))

(defun instruction-fast-math-flags (instruction)
  "Return LLVM's flags as a list and whether INSTRUCTION supports fast math."
  (let ((mask (%instruction-fast-math-flags instruction)))
    (cl:cond ((eql mask #xffffffff) (values nil nil))
             ((eql mask +all-fast-math-flags+) (values (list :fast) t))
             (t (values (cl:loop for flag in *fast-math-flags* for index from 0
                                 when (logbitp index mask) collect flag)
                        t)))))

(defun add-instruction-fast-math-flags (instruction flags)
  "Add FLAGS to INSTRUCTION's existing fast-math flags."
  (%add-instruction-fast-math-flags
   instruction (fast-math-flags-mask 'add-instruction-fast-math-flags flags)))

(defun block-instructions (block)
  "Return the instructions in BLOCK in LLVM order, including its terminator."
  (cl:loop for instruction = (block-first-instruction block) then (instruction-next instruction)
           while instruction collect instruction))

(defun find-instruction (function name)
  "Find a named instruction in FUNCTION, or return NIL."
  (check-type name string)
  (dolist (block (function-blocks function))
    (cl:loop for instruction = (block-first-instruction block)
             then (instruction-next instruction)
             while instruction
             when (equal name (instruction-name instruction))
               do (return-from find-instruction instruction))))

(defun instruction-opcode (instruction)
  "Return the LLVM opcode as a keyword, such as :ADD, :PHI, or :RET."
  (intern (string-upcase (%instruction-opcode instruction)) :keyword))

(defun instruction-operands (instruction)
  "Return the operands in LLVM order. Branch destinations are also available as blocks."
  (cl:loop for index below (%instruction-operand-count instruction)
           collect (instruction-operand instruction index)))

(defun instruction-successors (instruction)
  "Return the terminator's successor blocks in LLVM order."
  (cl:loop for index below (%instruction-successor-count instruction)
           collect (%instruction-successor instruction index)))

(defun phi-incomings (instruction)
  "Return a list of (VALUE PREDECESSOR) pairs for a PHI instruction."
  (cl:loop for index from 0
           for block = (%phi-incoming-block instruction index)
           while block collect (list (instruction-operand instruction index) block)))

(defun replace-instruction-operand (instruction index value)
  "Replace a supported data operand at INDEX. Verify the edit and restore it on failure."
  (%replace-instruction-operand instruction index value))

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (cl:if (handle-alive-p object)
                         (string-trim '(#\Space #\Tab #\Newline) (instruction-ir object))
                         (handle-status-label object))
                  stream)))

(define-operation begin-edit "begin_edit" :module-edit (module :handle))
(define-operation finish-edit "finish_edit" :module (edit :handle))
(define-operation (%draft-instruction draft-instruction) "edit_draft_instruction" :instruction
    (edit :handle) (instruction :handle))
(define-operation (%draft-value draft-value) "edit_draft_value" :value
    (edit :handle) (source :handle))

(defun draft-instruction (edit instruction)
  "Return the draft counterpart of an instruction from EDIT's source module."
  (%draft-instruction edit instruction))

(defun draft-value (edit source)
  "Resolve a source SSA value, constant, global, or function address in EDIT."
  (%draft-value edit (cl:or (operand-value source)
                            (error "Expected an LLVM operand, got ~S." source))))

(define-operation %edit-set-fast-math-flags "edit_set_fast_math_flags" :effect
    (edit :handle) (instruction :handle) (flags :uint32))

(defun edit-set-fast-math-flags (edit instruction flags)
  "Replace INSTRUCTION's fast-math flags in EDIT's private module copy.
FLAGS is a keyword or list. NIL clears all flags. Original handles identify the
same instruction across repeated edits. FINISH-EDIT returns the verified module."
  (%edit-set-fast-math-flags
   edit instruction (fast-math-flags-mask 'edit-set-fast-math-flags flags)))

(define-operation edit-replace-operand "edit_replace_operand" :effect
    (edit :handle) (instruction :handle) (index :uint32) (value :handle))
(define-operation edit-replace-all-uses "edit_replace_all_uses" :uint64
    (edit :handle) (source :handle) (replacement :handle))
(define-operation edit-move-before "edit_move_before" :effect
    (edit :handle) (instruction :handle) (anchor :handle))
(define-operation edit-move-after "edit_move_after" :effect
    (edit :handle) (instruction :handle) (anchor :handle))
(define-operation (%edit-detach-instruction edit-detach-instruction) "edit_detach_instruction" :effect
    (edit :handle) (instruction :handle))
(define-operation edit-insert-instruction-before "edit_insert_instruction_before" :effect
    (edit :handle) (instruction :handle) (anchor :handle))
(define-operation edit-insert-instruction-after "edit_insert_instruction_after" :effect
    (edit :handle) (instruction :handle) (anchor :handle))
(define-operation edit-erase-instruction "edit_erase_instruction" :effect
    (edit :handle) (instruction :handle))
(define-operation edit-move-block-before "edit_move_block_before" :effect
    (edit :handle) (block :handle) (anchor :handle))
(define-operation edit-move-block-after "edit_move_block_after" :effect
    (edit :handle) (block :handle) (anchor :handle))
(define-operation (%edit-detach-block edit-detach-block) "edit_detach_block" :effect
    (edit :handle) (block :handle))
(define-operation edit-insert-block-before "edit_insert_block_before" :effect
    (edit :handle) (block :handle) (anchor :handle))
(define-operation edit-insert-block-after "edit_insert_block_after" :effect
    (edit :handle) (block :handle) (anchor :handle))
(define-operation edit-erase-block "edit_erase_block" :effect
    (edit :handle) (block :handle))
(define-operation (%edit-redirect-successor edit-redirect-successor) "edit_redirect_successor" :effect
    (edit :handle) (terminator :handle) (index :uint32) (destination :handle)
    (incoming :pointer) (count :uintptr))

(defun edit-redirect-successor (edit terminator index destination &optional incoming)
  "Redirect a branch or switch edge, supplying one value per PHI in DESTINATION."
  (call-with-handles incoming
    (lambda (data count)
      (%edit-redirect-successor edit terminator index destination data count))))

(defun edit-detach-instruction (edit instruction)
  "Mark INSTRUCTION for reinsertion in the draft. FINISH-EDIT requires reinserting it first."
  (%edit-detach-instruction edit instruction))

(defun edit-detach-block (edit block)
  "Mark BLOCK for reinsertion in the draft. Its CFG edges remain attached during the move."
  (%edit-detach-block edit block))

(defmacro with-module-edit ((var module) &body body)
  "Apply BODY's edits to a private copy of MODULE and return the verified copy."
  (let ((edit (gensym "EDIT")))
    `(let ((,edit (begin-edit ,module)))
       (unwind-protect
            (progn
              (let ((,var ,edit))
                (declare (ignorable ,var))
                ,@body)
              (finish-edit ,edit))
         (release ,edit)))))

(define-operation copy-module "copy_module" :module (module :handle))

(defmacro with-module-copy ((var module) &body body)
  "Build on a copy of MODULE in a fresh context. Return the copy after verification."
  (let ((copy (gensym "COPY"))
        (verified (gensym "VERIFIED")))
    `(let ((,copy (copy-module ,module)) (,verified nil))
       (unwind-protect
            (let ((,var ,copy)
                  (*module* ,copy)
                  (*context* (module-context ,copy))
                  (*function* nil)
                  (*builder* nil))
              (declare (ignorable ,var))
              ,@body
              (verify-module ,copy)
              (setf ,verified t)
              ,copy)
         (cl:unless ,verified (release ,copy))))))

(define-operation %switch-case-count "instruction_switch_case_count" :uint64
    (instruction :handle))
(define-operation %switch-case-value "instruction_switch_case_value" :value
    (instruction :handle) (index :uint32))
(define-operation %switch-case-block "instruction_switch_case_block" :block
    (instruction :handle) (index :uint32))

(defun switch-cases (instruction)
  "Return the explicit (KEY DESTINATION) pairs in switch order."
  (cl:loop for index below (%switch-case-count instruction)
           collect (list (%switch-case-value instruction index)
                         (%switch-case-block instruction index))))

(define-operation (%edit-add-switch-case edit-add-switch-case) "edit_add_switch_case" :effect
    (edit :handle) (instruction :handle) (key :handle) (destination :handle)
    (incoming :pointer) (count :uintptr))

(defun edit-add-switch-case (edit instruction key destination &optional incoming)
  "Append constant integer KEY to the switch, with one incoming value per destination PHI."
  (call-with-handles incoming
    (lambda (data count)
      (%edit-add-switch-case edit instruction key destination data count))))

(define-operation edit-remove-switch-case "edit_remove_switch_case" :effect
    (edit :handle) (instruction :handle) (key :handle))
