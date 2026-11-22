(in-package #:zk-llvm)

(define-operation (%make-debug-info make-debug-info) "create_debug_compile_unit" :debug-info
    (module :handle) (filename :cstring) (directory :cstring) (producer :cstring)
    (language :cstring) (optimized :bool) (emission :uint32) (flags :cstring)
    (runtime-version :uint32) (debug-info-for-profiling :bool) (sysroot :cstring) (sdk :cstring))
(define-operation (%make-debug-file make-debug-file) "make_debug_file" :debug-file
    (info :handle) (filename :cstring) (directory :cstring))
(define-operation debug-file-filename "debug_file_filename" :text (file :handle))
(define-operation debug-file-directory "debug_file_directory" :text (file :handle))
(define-operation (%attach-function-debug-info attach-function-debug-info)
    "attach_debug_subprogram" :debug-scope
    (info :handle) (function :handle) (line :uint32) (name :cstring) (file :optional-handle)
    (result :optional-handle) (parameters :pointer) (count :uintptr) (variadic :bool))
(define-operation (%make-debug-lexical-block make-debug-lexical-block) "make_debug_lexical_block" :debug-scope
    (info :handle) (scope :handle) (line :uint32) (column :uint16) (file :optional-handle))
(define-operation function-debug-scope "function_debug_scope" :debug-scope (function :handle))
(define-operation debug-scope-line "debug_scope_line" :uint64 (scope :handle))
(define-operation debug-scope-file "debug_scope_file" :debug-file (scope :handle))
(define-operation (%make-debug-location make-debug-location) "make_debug_location" :debug-location
    (scope :handle) (line :uint32) (column :uint16))
(define-operation (%make-debug-inlined-location make-debug-inlined-location) "make_debug_inlined_location"
    :debug-location (scope :handle) (line :uint32) (column :uint16) (inlined-at :handle))
(define-operation debug-location-line "debug_location_line" :uint64 (location :handle))
(define-operation debug-location-column "debug_location_column" :uint64 (location :handle))
(define-operation debug-location-file "debug_location_file" :debug-file (location :handle))
(define-operation debug-location-inlined-at "debug_location_inlined_at" :debug-location (location :handle))
(define-operation set-debug-location "set_debug_location" :effect
    (builder :handle) (location :optional-handle))
(define-operation builder-debug-location "builder_debug_location" :debug-location (builder :handle))
(define-operation instruction-debug-location "instruction_debug_location" :debug-location
    (instruction :handle))
(define-operation set-instruction-debug-location "set_instruction_debug_location" :effect
    (instruction :handle) (location :optional-handle))
(define-operation (%finish-debug-function finish-debug-function) "finish_debug_function" :effect
    (info :handle) (function :handle))
(define-operation finish-debug-info "finish_debug_info" :effect (info :handle))
(define-operation (%make-debug-type make-debug-type) "make_debug_type" :debug-type
    (info :handle) (name :cstring) (bits :uint64) (encoding :uint32))
(define-operation (%make-debug-pointer-type make-debug-pointer-type) "make_debug_pointer_type" :debug-type
    (info :handle) (pointee :optional-handle) (bits :uint64) (align-bits :uint32)
    (address-space :uint32) (name :cstring))
(define-operation (%make-debug-array-type make-debug-array-type) "make_debug_array_type" :debug-type
    (info :handle) (element :handle) (bits :uint64) (align-bits :uint32) (count :uint64))
(define-operation (%make-debug-member make-debug-member) "make_debug_member" :debug-member
    (name :cstring) (type :handle) (bits :uint64) (align-bits :uint32) (offset-bits :uint64))
(define-operation (%make-debug-struct-type make-debug-struct-type) "make_debug_struct_type" :debug-type
    (info :handle) (name :cstring) (bits :uint64) (align-bits :uint32) (line :uint32)
    (file :optional-handle) (members :pointer) (count :uintptr))
(define-operation (%make-debug-union-type make-debug-union-type) "make_debug_union_type" :debug-type
    (info :handle) (name :cstring) (bits :uint64) (align-bits :uint32) (line :uint32)
    (file :optional-handle) (members :pointer) (count :uintptr))
(define-operation (%make-debug-variable make-debug-variable) "describe_debug_variable" :debug-variable
    (info :handle) (scope :handle) (name :cstring) (type :handle) (line :uint32) (parameter :uint16))
(define-operation (%record-debug-value record-debug-value) "insert_debug_value_before" :effect
    (info :handle) (block :handle) (variable :handle) (value :handle) (location :handle)
    (before :optional-handle))
(define-operation (%record-debug-address record-debug-address) "insert_debug_address_before" :effect
    (info :handle) (block :handle) (variable :handle) (address :handle) (location :handle)
    (before :optional-handle))

(defun make-debug-info (module filename &key (directory "") (producer "zkcl-llvm-api")
                                            (language :c) optimized (emission :line-tables)
                                            (flags "") (runtime-version 0) debug-info-for-profiling
                                            (sysroot "") (sdk ""))
  "Open a compilation unit. Finish it before verifying MODULE.
LANGUAGE is a DWARF source language keyword, for example :C, :C++, :RUST, or :OCAML."
  (cl:unless (keywordp language)
    (error 'llvm-error :operation 'make-debug-info
           :message (format nil "Expected a DWARF source language keyword, got ~S." language)))
  (%make-debug-info module filename directory producer
                    (string-downcase (symbol-name language)) optimized
                    (operation-index 'make-debug-info emission '(:line-tables :full))
                    flags runtime-version debug-info-for-profiling sysroot sdk))

(defmacro with-debug-info ((var module filename &rest options) &body body)
  "Create a debug compilation unit and finalize it when BODY exits."
  (let ((info (gensym "DEBUG-INFO")))
    `(let* ((,info (make-debug-info ,module ,filename ,@options))
            (,var ,info))
       (unwind-protect
            (locally ,@body)
         (cl:when (handle-open-p ,info)
           (finish-debug-info ,info))))))

(defun make-debug-file (info filename &key (directory ""))
  "Describe another source file for this debug compilation unit."
  (%make-debug-file info filename directory))

(defun attach-function-debug-info (info function line &key (name "") file result parameters variadic)
  "Attach a source definition to FUNCTION and return its debug scope."
  (call-with-handles parameters
    (lambda (data count)
      (%attach-function-debug-info info function line name file result data count variadic))))

(defun make-debug-lexical-block (info scope line &key (column 0) file)
  "Create a lexical scope within SCOPE. FILE defaults to its file."
  (%make-debug-lexical-block info scope line column file))

(defun finish-debug-function (info function)
  "Finish creating FUNCTION's debug variables in INFO. Existing variables can still appear in debug records."
  (%finish-debug-function info function))

(defun make-debug-location (scope line &optional (column 0))
  "Describe a source location. Column zero denotes an unknown column."
  (%make-debug-location scope line column))

(defun make-debug-inlined-location (scope line inlined-at &optional (column 0))
  "Describe code from SCOPE inlined at the caller's source location INLINED-AT."
  (%make-debug-inlined-location scope line column inlined-at))

(defmacro with-debug-location ((builder location) &body body)
  "Emit BODY with BUILDER's current source LOCATION, then restore its previous one.
NIL LOCATION suppresses source locations. Both forms are evaluated once, in order.
BODY must leave BUILDER positioned in the same function. Set a location again
when entering a new block because changing the insertion point clears it."
  (let ((engine (gensym "BUILDER"))
        (location-value (gensym "LOCATION"))
        (previous (gensym "PREVIOUS-LOCATION")))
    `(let* ((,engine ,builder)
            (,location-value ,location)
            (,previous (builder-debug-location ,engine)))
       (unwind-protect
            (progn
              (set-debug-location ,engine ,location-value)
              (locally ,@body))
         (unwind-protect (set-debug-location ,engine ,previous)
           (cl:when ,previous (release ,previous)))))))

(defun make-debug-type (info name bits encoding)
  "Describe a scalar source-language type to the debugger."
  (%make-debug-type info name bits
                    (operation-index 'make-debug-type encoding
                                     '(:address :boolean :float :signed :unsigned))))

(defun make-debug-pointer-type (info pointee bits &key (align-bits 0) (address-space 0) (name ""))
  "Describe a pointer with the supplied bit size, alignment, and DWARF ADDRESS-SPACE.
NIL POINTEE denotes void."
  (%make-debug-pointer-type info pointee bits align-bits address-space name))

(defun make-debug-array-type (info element count bits &key (align-bits 0))
  "Describe a fixed-length array of COUNT elements with the supplied bit size and alignment."
  (%make-debug-array-type info element bits align-bits count))

(defun make-debug-member (name type bits offset-bits &key (align-bits 0))
  "Describe a structure or union member using a bit size, alignment, and offset."
  (%make-debug-member name type bits align-bits offset-bits))

(defun make-debug-struct-type (info name bits members &key (align-bits 0) (line 0) file)
  "Describe a structure with explicit size, alignment, and member offsets in bits."
  (call-with-handles members
    (lambda (data count)
      (%make-debug-struct-type info name bits align-bits line file data count))))

(defun make-debug-union-type (info name bits members &key (align-bits 0) (line 0) file)
  "Describe a union with explicit size and alignment in bits. Members start at offset zero."
  (call-with-handles members
    (lambda (data count)
      (%make-debug-union-type info name bits align-bits line file data count))))

(defun make-debug-variable (info scope name type line &key parameter)
  "Describe a local variable or the zero-based PARAMETER of a function."
  (cl:when parameter (check-type parameter (integer 0 65534)))
  (%make-debug-variable info scope name type line (cl:if parameter (cl:1+ parameter) 0)))

(defun record-debug-value (info block variable value location &key before)
  "Describe VARIABLE before an instruction. BEFORE defaults to BLOCK's terminator."
  (%record-debug-value info block variable value location before))

(defun record-debug-address (info block variable address location &key before)
  "Declare VARIABLE's storage ADDRESS before an instruction or BLOCK's terminator."
  (%record-debug-address info block variable address location before))
