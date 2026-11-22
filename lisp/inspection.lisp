(in-package #:zk-llvm)

(define-operation block-last-instruction "block_last_instruction" :instruction (block :handle))
(define-operation instruction-previous "instruction_previous" :instruction (instruction :handle))
(define-operation instruction-equal-p "instruction_eq" :bool (left :handle) (right :handle))
(define-operation function-equal-p "function_eq" :bool (left :handle) (right :handle))
(define-operation global-equal-p "global_eq" :bool (left :handle) (right :handle))

(define-operation (%block-successors block-successors) "block_successors" :pointer (block :handle))
(define-operation (%block-predecessors block-predecessors) "block_predecessors" :pointer (block :handle))
(define-operation (%function-reverse-postorder function-reverse-postorder)
    "function_reverse_postorder" :pointer (function :handle))
(define-operation %block-list-count "block_list_count" :uint64 (blocks :pointer))
(define-operation %block-list-at "block_list_at" :block (blocks :pointer) (index :uintptr))

(define-operation (%value-argument-function value-argument-function)
    "value_argument_function" :function (value :handle))
(define-operation (%value-argument-index value-argument-index)
    "value_argument_index" :uint64 (value :handle))

(define-operation (%instruction-icmp-predicate instruction-icmp-predicate)
    "instruction_icmp_predicate" :uint64 (instruction :handle))
(define-operation (%instruction-fcmp-predicate instruction-fcmp-predicate)
    "instruction_fcmp_predicate" :uint64 (instruction :handle))

(define-operation value-name "value_name" :text (value :handle))
(define-operation instruction-name "instruction_name" :text (instruction :handle))
(define-operation set-value-name "set_value_name" :effect (value :handle) (name :cstring))
(define-operation set-instruction-name "set_instruction_name" :effect
    (instruction :handle) (name :cstring))
(define-operation set-block-name "set_block_name" :effect (block :handle) (name :cstring))
(define-operation set-function-name "set_function_name" :effect (function :handle) (name :cstring))
(define-operation set-global-name "set_global_name" :effect (global :handle) (name :cstring))

(define-operation (%function-unnamed-address function-unnamed-address)
    "function_unnamed_address" :uint64 (function :handle))
(define-operation (%set-function-unnamed-address set-function-unnamed-address)
    "set_function_unnamed_address" :effect (function :handle) (mode :uint32))

(defun instruction-icmp-predicate (instruction)
  "Return the ICMP predicate keyword, or NIL for another opcode."
  (let ((index (%instruction-icmp-predicate instruction)))
    (cl:unless (zerop index)
      (nth (1- index) *int-predicates*))))

(defun instruction-fcmp-predicate (instruction)
  "Return the FCMP predicate keyword, or NIL for another opcode."
  (let ((index (%instruction-fcmp-predicate instruction)))
    (cl:unless (zerop index)
      (nth (1- index) *float-predicates*))))

(defun function-unnamed-address (function)
  (nth (%function-unnamed-address function) *unnamed-address-modes*))

(defun set-function-unnamed-address (function mode)
  (%set-function-unnamed-address function
    (operation-index 'set-function-unnamed-address mode *unnamed-address-modes*)))

(defun collect-block-list (blocks)
  (unwind-protect
      (cl:loop for index below (%block-list-count blocks) collect (%block-list-at blocks index))
    (%release blocks)))

(defun block-successors (block)
  "Return one destination per outgoing edge in the terminator's successor order."
  (collect-block-list (%block-successors block)))

(defun block-predecessors (block)
  "Return one source per incoming edge in function block order, including parallel edges."
  (collect-block-list (%block-predecessors block)))

(defun function-reverse-postorder (function)
  "Return the blocks reachable from FUNCTION's entry in reverse postorder.
Depth-first traversal follows the terminator's successor order."
  (collect-block-list (%function-reverse-postorder function)))

(defun value-argument-function (value)
  "Return the enclosing function of an argument, or NIL for other values."
  (%value-argument-function value))

(defun value-argument-index (value)
  "Return the zero-based index of an argument, or NIL for other values."
  (let ((index (%value-argument-index value)))
    (cl:unless (eql index #xffffffffffffffff)
      index)))

(zk:defpattern instruction-opcode (pattern)
  `(cl:and (type instruction) (zk:view instruction-opcode ,pattern)))

(zk:defpattern instruction (opcode &optional (operands nil operands-p))
  `(cl:and (instruction-opcode ,opcode)
           ,@(cl:when operands-p `((zk:view instruction-operands ,operands)))))

(zk:defpattern value-kind (pattern)
  `(cl:and (type value) (zk:view value-kind ,pattern)))
