(in-package #:zk-llvm)

(define-operation (%metadata-string metadata-string) "metadata_string" :metadata
    (context :handle) (string :cstring))
(define-operation (%metadata-node metadata-node) "metadata_node" :metadata
    (context :handle) (operands :pointer) (count :uintptr))
(define-operation value-metadata "value_metadata" :metadata (value :handle))
(define-operation metadata-ir "metadata_ir" :text (metadata :handle))
(define-operation metadata-string-value "metadata_string_value" :optional-text (metadata :handle))
(define-operation (%metadata-integer-value metadata-integer-value) "metadata_integer_value" :optional-text
    (metadata :handle))
(define-operation metadata-operand-count "metadata_operand_count" :uint64 (metadata :handle))
(define-operation metadata-operand "metadata_operand" :metadata (metadata :handle) (index :uint32))
(define-operation module-add-named-metadata "module_add_named_metadata" :effect
    (module :handle) (name :cstring) (node :handle))
(define-operation module-named-metadata-count "module_named_metadata_count" :uint64
    (module :handle) (name :cstring))
(define-operation module-named-metadata-operand "module_named_metadata_operand" :metadata
    (module :handle) (name :cstring) (index :uint32))
(define-operation (%module-flag module-flag) "module_flag" :metadata
    (module :handle) (name :cstring) (behavior :pointer))
(define-operation (%module-add-flag module-add-flag) "module_add_flag" :effect
    (module :handle) (name :cstring) (behavior :uint32) (value :handle))
(define-operation module-strip-debug-info "module_strip_debug_info" :module (module :handle))
(define-operation module-identifier "module_identifier" :text (module :handle))
(define-operation module-set-identifier "module_set_identifier" :effect (module :handle) (name :cstring))
(define-operation module-source-filename "module_source_filename" :text (module :handle))
(define-operation module-set-source-filename "module_set_source_filename" :effect
    (module :handle) (filename :cstring))
(define-operation module-inline-asm "module_inline_asm" :text (module :handle))
(define-operation module-set-inline-asm "module_set_inline_asm" :effect (module :handle) (text :cstring))
(define-operation module-append-inline-asm "module_append_inline_asm" :effect (module :handle) (text :cstring))

(defun metadata-string (context-or-string &optional (string nil string-p))
  "Create an LLVM metadata string. Omit CONTEXT to use *CONTEXT*."
  (cl:if string-p
         (%metadata-string context-or-string string)
         (%metadata-string (ensure-context) context-or-string)))

(defun metadata-node (context-or-operands &optional (operands nil operands-p))
  "Create a metadata tuple from OPERANDS. NIL represents a null operand."
  (let ((context (cl:if operands-p context-or-operands (ensure-context)))
        (items (cl:if operands-p operands context-or-operands)))
    (call-with-handles items (lambda (data count) (%metadata-node context data count))
                            :allow-nil t)))

(defun metadata-integer-value (metadata)
  "Return the integer metadata as a signed LLVM integer, or NIL for other metadata."
  (let ((text (%metadata-integer-value metadata)))
    (cl:when text (parse-integer text :radix 10))))

(defun metadata-operands (node)
  "Return the operands of a metadata tuple as a list."
  (cl:loop for index below (metadata-operand-count node)
           collect (metadata-operand node index)))

(defun module-named-metadata (module name)
  "Return the tuple entries in the named metadata list NAME."
  (cl:loop for index below (module-named-metadata-count module name)
           collect (module-named-metadata-operand module name index)))

(defparameter *module-flag-behaviors*
  '(:error :warning :require :override :append :append-unique :max :min))

(defun module-flag (module name)
  "Return the metadata and link behavior of a module flag, or NIL if absent."
  (ffi:with-foreign-memory (behavior (ffi:type-size :uint32))
    (let ((value (%module-flag module name behavior)))
      (cl:if value
             (values value (nth (cl:1- (ffi:mem-ref behavior :uint32)) *module-flag-behaviors*))
             (values nil nil)))))

(defun module-add-flag (module name behavior value)
  "Record a module flag with a metadata VALUE and an LLVM link BEHAVIOR."
  (%module-add-flag module name (cl:1+ (operation-index 'module-add-flag behavior *module-flag-behaviors*))
                    value))
