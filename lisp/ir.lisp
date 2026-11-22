(in-package #:zk-llvm)

(define-operation make-context "make_context" :context)
(define-operation (%make-module make-module) "make_module" :module (context :handle) (name :cstring))
(define-operation (%make-builder make-builder) "make_builder" :builder (context :handle))
(define-operation (%parse-ir parse-ir) "parse_ir" :module (context :handle) (text :cstring))
(define-operation (%parse-bitcode parse-bitcode) "parse_bitcode" :module
    (context :handle) (data :pointer) (count :uintptr))
(define-operation (%module-context module-context) "module_context" :context (module :handle))
(define-operation module-ir "module_ir" :text (module :handle))
(define-operation module-bitcode "module_bitcode" :bytes (module :handle))
(define-operation verify-module "verify_module" :verified (module :handle))
(define-operation (%link-modules link-modules) "link_modules" :module
    (primary :handle) (inputs :pointer) (count :uintptr))

(defun ensure-context (&optional (context *context*))
  (cl:or context (error "No active LLVM context. Pass a context or use WITH-CONTEXT / WITH-MODULE.")))

(defun ensure-module (&optional (module *module*))
  (cl:or module (error "No active LLVM module. Pass a module or use WITH-MODULE / DEFINE-MODULE.")))

(defun make-module (context-or-name &optional (name nil name-p))
  (let* ((context (cl:if name-p context-or-name (ensure-context)))
         (module-name (cl:if name-p name context-or-name))
         (module (%make-module context module-name)))
    (setf (handle-owner module) context)
    module))

(defun make-builder (&optional (context (ensure-context)))
  (let ((builder (%make-builder context)))
    (setf (handle-owner builder) context)
    builder))

(defun parse-ir (context-or-text &optional (text nil text-p))
  (let* ((context (cl:if text-p context-or-text (ensure-context)))
         (source (cl:if text-p text context-or-text))
         (module (%parse-ir context source)))
    (setf (handle-owner module) context)
    module))

(defun parse-bitcode (context-or-bytes &optional (bytes nil bytes-p))
  (let* ((context (cl:if bytes-p context-or-bytes (ensure-context)))
         (octets (cl:if bytes-p bytes context-or-bytes))
         (module (call-with-bytes octets
                   (lambda (data count) (%parse-bitcode context data count)))))
    (setf (handle-owner module) context)
    module))

(defun module-context (module)
  (let ((context (handle-owner module)))
    (cl:if (handle-open-p context)
           context
           (setf (handle-owner module) (%module-context module)))))

(defun link-modules (primary &rest modules)
  (call-with-handles modules (lambda (data count) (%link-modules primary data count))))

(define-operation (%int-type int-type) "int_type" :type (context :handle) (width :uint32))
(define-operation (%float-type float-type) "float_type" :type (context :handle))
(define-operation (%double-type double-type) "double_type" :type (context :handle))
(define-operation %extended-float-type "extended_float_type" :type
    (context :handle) (kind :uint32))
(define-operation (%void-type void-type) "void_type" :type (context :handle))
(define-operation (%label-type label-type) "label_type" :type (context :handle))
(define-operation (%metadata-type metadata-type) "metadata_type" :type (context :handle))
(define-operation (%token-type token-type) "token_type" :type (context :handle))
(define-operation (%pointer-type pointer-type) "pointer_type_in_address_space" :type
    (context :handle) (address-space :uint32))
(define-operation (%array-type array-type) "array_type_with_length" :type
    (element :handle) (count :uint64))
(define-operation (%vector-type vector-type) "vector_type" :type
    (element :handle) (count :uint32) (scalable :bool))
(define-operation (%struct-type struct-type) "struct_type" :type
    (context :handle) (fields :pointer) (count :uintptr) (packed :bool))
(define-operation (%opaque-struct-type opaque-struct-type) "opaque_struct_type" :type
    (context :handle) (name :cstring))
(define-operation (%find-struct-type find-struct-type) "find_struct_type" :type
    (context :handle) (name :cstring))
(define-operation (%set-struct-body set-struct-body) "set_struct_body" :effect
    (type :handle) (fields :pointer) (count :uintptr) (packed :bool))
(define-operation (%function-type function-type) "function_type" :type
    (result :handle) (parameters :pointer) (count :uintptr) (variadic :bool))
(define-operation type-ir "type_ir" :text (type :handle))
(define-operation %type-context "type_context" :context (type :handle))
(define-operation type-equal-p "type_eq" :bool (left :handle) (right :handle))
(define-operation (%type-kind type-kind) "type_kind_id" :uint64 (type :handle))
(define-operation (%type-subtype type-subtype) "type_subtype" :type (type :handle) (index :uint32))
(define-operation (%type-subtype-count type-subtype-count) "type_subtype_count" :uint64 (type :handle))
(define-operation (%scalar-type scalar-type) "scalar_type" :type (type :handle))
(define-operation (%type-sized-p type-sized-p) "type_is_sized" :bool (type :handle))
(define-operation (%type-floating-point-p type-floating-point-p) "type_is_floating_point" :bool (type :handle))
(define-operation (%floating-point-width floating-point-width) "type_floating_point_width" :uint64 (type :handle))
(define-operation integer-width "integer_bit_width" :uint64 (type :handle))
(define-operation pointer-address-space "pointer_address_space_id" :uint64 (type :handle))
(define-operation array-length "array_length" :uint64 (type :handle))
(define-operation vector-length "vector_length" :uint64 (type :handle))
(define-operation struct-name "struct_name" :optional-text (type :handle))
(define-operation struct-opaque-p "struct_is_opaque" :bool (type :handle))
(define-operation struct-packed-p "struct_is_packed" :bool (type :handle))
(define-operation function-type-variadic-p "function_type_is_variadic" :bool (type :handle))
(define-operation value-type "value_type" :type (value :handle))
(define-operation value-ir "value_ir" :text (value :handle))
(define-operation value-constant-p "value_is_constant" :bool (value :handle))
(define-operation (%const-int const-int) "const_int" :value (type :handle) (integer :cstring))
(define-operation (%constant-integer constant-integer) "constant_integer" :text
    (value :handle) (signed :bool))
(define-operation (%const-float const-float) "const_float" :value (type :handle) (number :double))
(define-operation (%const-float-from-string const-float-from-string) "const_float_from_string" :value
    (type :handle) (literal :cstring))
(define-operation (%const-zero const-zero) "const_zero" :value (type :handle))
(define-operation (%const-undef const-undef) "const_undef" :value (type :handle))
(define-operation (%const-poison const-poison) "const_poison" :value (type :handle))
(define-operation (%const-bytes const-bytes) "const_bytes" :value
    (context :handle) (data :pointer) (count :uintptr) (nul-terminated :bool))
(define-operation (%const-array const-array) "const_array" :value
    (element-type :handle) (elements :pointer) (count :uintptr))
(define-operation (%const-struct const-struct) "const_struct" :value
    (type :handle) (fields :pointer) (count :uintptr))
(define-operation (%const-vector const-vector) "const_vector" :value
    (type :handle) (elements :pointer) (count :uintptr))
(define-operation (%const-splat const-splat) "const_splat" :value
    (type :handle) (element :handle))
(define-operation (%const-gep const-gep) "const_gep" :value
    (type :handle) (address :handle) (indices :pointer) (count :uintptr) (in-bounds :bool))

(defun remember-type-context (type context)
  (cl:when (cl:and type context)
    (setf (handle-owner type) context))
  type)

(defun type-context (type)
  (let ((context (handle-owner type)))
    (cl:if (handle-open-p context)
           context
           (setf (handle-owner type) (%type-context type)))))

(defun int-type (context-or-width &optional (width nil width-p))
  (let ((context (cl:if width-p context-or-width (ensure-context)))
        (bits (cl:if width-p width context-or-width)))
    (remember-type-context (%int-type context bits) context)))

(defun float-type (&optional (context (ensure-context)))
  (remember-type-context (%float-type context) context))

(defun double-type (&optional (context (ensure-context)))
  (remember-type-context (%double-type context) context))

(defun extended-float-type (context kind)
  (remember-type-context
   (%extended-float-type context (position kind '(:half :bfloat :x86-fp80 :fp128 :ppc-fp128)))
   context))

(defun half-type (&optional (context (ensure-context)))
  (extended-float-type context :half))

(defun bfloat-type (&optional (context (ensure-context)))
  (extended-float-type context :bfloat))

(defun x86-fp80-type (&optional (context (ensure-context)))
  (extended-float-type context :x86-fp80))

(defun fp128-type (&optional (context (ensure-context)))
  (extended-float-type context :fp128))

(defun ppc-fp128-type (&optional (context (ensure-context)))
  (extended-float-type context :ppc-fp128))

(defun void-type (&optional (context (ensure-context)))
  (remember-type-context (%void-type context) context))

(defun label-type (&optional (context (ensure-context)))
  (remember-type-context (%label-type context) context))

(defun metadata-type (&optional (context (ensure-context)))
  (remember-type-context (%metadata-type context) context))

(defun token-type (&optional (context (ensure-context)))
  (remember-type-context (%token-type context) context))

(defun pointer-type (&rest options)
  "Construct an opaque pointer type in the optional context and address space."
  (let ((context (cl:if (cl:and options (cl:not (keywordp (first options))))
                        (pop options)
                        (ensure-context))))
    (destructuring-bind (&key (address-space 0)) options
      (remember-type-context (%pointer-type context address-space) context))))

(defun parse-integer-type-keyword (keyword)
  (let ((name (symbol-name keyword)))
    (cl:when (cl:and (cl:>= (length name) 2)
                     (cl:or (char= (char name 0) #\I) (char= (char name 0) #\U))
                     (every #'digit-char-p (subseq name 1)))
      (parse-integer name :start 1))))

(defun resolve-type (spec &optional (context *context*))
  "Resolve an LLVM type handle or symbolic type specifier in CONTEXT."
  (zk:match spec
    ((type ir-type) spec)
    (:void (void-type (ensure-context context)))
    (:label (label-type (ensure-context context)))
    (:metadata (metadata-type (ensure-context context)))
    (:token (token-type (ensure-context context)))
    (:ptr (pointer-type (ensure-context context)))
    (:bool (int-type (ensure-context context) 1))
    ((cl:or :f32 :float) (float-type (ensure-context context)))
    ((cl:or :f64 :double) (double-type (ensure-context context)))
    ((cl:or :half :bfloat :x86-fp80 :fp128 :ppc-fp128)
     (extended-float-type (ensure-context context) spec))
    ((type keyword)
     (let ((bits (parse-integer-type-keyword spec)))
       (cl:if bits
              (int-type (ensure-context context) bits)
              (error 'llvm-error :operation 'resolve-type
                     :message (format nil "Unknown LLVM type keyword ~S." spec)))))
    ((type string)
     (cl:or (find-struct-type (ensure-context context) spec)
            (error 'llvm-error :operation 'resolve-type
                   :message (format nil "Unknown LLVM struct type ~S." spec))))
    ((list :int bits)
     (int-type (ensure-context context) bits))
    ((list :ptr)
     (pointer-type (ensure-context context) :address-space 0))
    ((list :ptr address-space)
     (pointer-type (ensure-context context) :address-space address-space))
    ((list :array element count)
     (let ((ctx (ensure-context context)))
       (array-type (resolve-type element ctx) count)))
    ((list (cl:or :vector :scalable-vector) element count)
     (vector-type (resolve-type element context) count :scalable (eq (first spec) :scalable-vector)))
    ((list* :struct fields)
     (let ((ctx (ensure-context context)))
       (struct-type ctx (mapcar (lambda (field) (resolve-type field ctx)) fields))))
    ((list* :packed-struct fields)
     (let ((ctx (ensure-context context)))
       (struct-type ctx (mapcar (lambda (field) (resolve-type field ctx)) fields) :packed t)))
    ((list* (cl:or :fn :function) result parameters options)
     (let ((ctx (ensure-context context)))
       (destructuring-bind (&key variadic) options
         (function-type (resolve-type result ctx)
                        (mapcar (lambda (p) (resolve-type p ctx)) (sequence->list parameters))
                        :variadic variadic))))
    (_ (error 'llvm-error :operation 'resolve-type
              :message (format nil "Invalid LLVM type specifier ~S." spec)))))

(defun array-type (element count)
  (%array-type (resolve-type element) count))

(defun vector-type (element count &key scalable)
  "Construct a vector of scalar ELEMENT values. COUNT is the minimum length when SCALABLE."
  (%vector-type (resolve-type element) count scalable))

(defun struct-type (context-or-fields &rest options)
  "Construct a literal struct type, optionally taking a context before the fields."
  (let* ((explicit-context-p (context-p context-or-fields))
         (fields (cl:if explicit-context-p
                        (cl:if (cl:and options (cl:not (keywordp (first options))))
                               (pop options)
                               (error "LLVM:STRUCT-TYPE requires fields after the context."))
                        context-or-fields))
         (field-list (sequence->list fields))
         (context (ensure-context
                   (cl:if explicit-context-p context-or-fields
                          (infer-type-list-context nil field-list))))
         (resolved (mapcar (lambda (f) (resolve-type f context)) field-list)))
    (destructuring-bind (&key packed) options
      (remember-type-context
       (call-with-handles resolved
         (lambda (data count) (%struct-type context data count packed)))
       context))))

(defun opaque-struct-type (context-or-name &optional (name nil name-p))
  (let ((context (cl:if name-p context-or-name (ensure-context)))
        (struct-name (cl:if name-p name context-or-name)))
    (remember-type-context (%opaque-struct-type context struct-name) context)))

(defun find-struct-type (context-or-name &optional (name nil name-p))
  (let ((context (cl:if name-p context-or-name (ensure-context)))
        (struct-name (cl:if name-p name context-or-name)))
    (remember-type-context (%find-struct-type context struct-name) context)))

(defun set-struct-body (type fields &key packed)
  (let* ((ctx (type-context type))
         (resolved (mapcar (lambda (f) (resolve-type f ctx)) (sequence->list fields))))
    (call-with-handles resolved
      (lambda (data count) (%set-struct-body type data count packed)))))

(defun infer-type-list-context (result parameters)
  (let ((type (cl:if (type-p result) result (find-if #'type-p parameters))))
    (cl:if type (type-context type) *context*)))

(defun function-type (result parameters &key variadic)
  (let* ((param-list (sequence->list parameters))
         (ctx (infer-type-list-context result param-list))
         (res (resolve-type result ctx))
         (params (mapcar (lambda (p) (resolve-type p ctx)) param-list)))
    (remember-type-context
     (call-with-handles params
       (lambda (data count) (%function-type res data count variadic)))
     ctx)))

(defun type-kind (type)
  (aref #(:void :half :float :double :x86-fp80 :fp128 :ppc-fp128 :label :integer
          :function :struct :array :pointer :vector :metadata nil :token :scalable-vector
          :bfloat :x86-amx :target-extension)
        (%type-kind (resolve-type type))))

(defun type-subtype (type index)
  (let* ((resolved (resolve-type type))
         (sub (%type-subtype resolved index)))
    (remember-type-context sub (type-context resolved))))

(defun type-subtype-count (type)
  (%type-subtype-count (resolve-type type)))

(defun type-subtypes (type)
  (let ((resolved (resolve-type type)))
    (cl:loop for index below (type-subtype-count resolved)
             collect (type-subtype resolved index))))

(defun scalar-type (type)
  "Return the element type of a fixed or scalable vector, or TYPE itself."
  (let ((resolved (resolve-type type)))
    (remember-type-context (%scalar-type resolved) (type-context resolved))))

(defun type-sized-p (type)
  "Return whether LLVM can compute the size of TYPE for a target data layout."
  (%type-sized-p (resolve-type type)))

(defun type-floating-point-p (type)
  "Return whether TYPE is a scalar floating-point type."
  (%type-floating-point-p (resolve-type type)))

(defun floating-point-width (type)
  "Return the number of bits in a scalar floating-point type."
  (%floating-point-width (resolve-type type)))

(defun type-spec (type)
  "Describe an LLVM type. Unconstructible types are described by their kind."
  (let* ((ty (resolve-type type))
         (kind (type-kind ty)))
    (case kind
      (:void :void)
      (:float :f32)
      (:double :f64)
      (:integer
       (let ((bits (integer-width ty)))
         (case bits
           (1 :i1) (8 :i8) (16 :i16) (32 :i32) (64 :i64) (128 :i128)
           (otherwise (list :int bits)))))
      (:pointer
       (let ((as (pointer-address-space ty)))
         (cl:if (zerop as) :ptr (list :ptr as))))
      (:array
       (list :array (type-spec (type-subtype ty 0)) (array-length ty)))
      ((:vector :scalable-vector)
       (list kind (type-spec (type-subtype ty 0)) (vector-length ty)))
      (:struct
       (cl:or (struct-name ty)
              (cons (cl:if (struct-packed-p ty) :packed-struct :struct)
                    (mapcar #'type-spec (type-subtypes ty)))))
      (:function
       (let ((subs (mapcar #'type-spec (type-subtypes ty))))
         (cl:if (function-type-variadic-p ty)
                (list :fn (first subs) (rest subs) :variadic t)
                (list :fn (first subs) (rest subs)))))
      (otherwise kind))))

(zk:defpattern type-spec (pattern)
  `(cl:and (type ir-type) (zk:view type-spec ,pattern)))

(defun const-int (type value)
  "Construct an integer constant, taking the low bits specified by TYPE."
  (check-type value integer)
  (let ((resolved (resolve-type type)))
    (%const-int resolved (format nil "~D" value))))

(defun constant-integer (value &key signed)
  "Read a scalar integer constant, using signed two's complement when SIGNED."
  (parse-integer (%constant-integer value signed)))

(defun const-float (type value)
  "Construct a floating-point constant from a real number or a decimal string."
  (etypecase value
    (real (%const-float (resolve-type type) (coerce value 'double-float)))
    (string (const-float-from-string type value))))

(defun const-float-from-string (type literal)
  "Parse an ASCII decimal literal directly in TYPE's floating-point format."
  (check-type literal string)
  (cl:when (position (code-char 0) literal)
    (error 'llvm-error :operation 'const-float-from-string
           :message "An LLVM floating-point literal cannot contain NUL."))
  (%const-float-from-string (resolve-type type) literal))

(defun const-zero (type)
  (%const-zero (resolve-type type)))

(defun const-undef (type)
  "Construct LLVM's undef value of TYPE."
  (%const-undef (resolve-type type)))

(defun const-poison (type)
  "Construct LLVM's poison value of TYPE."
  (%const-poison (resolve-type type)))

(defun const-bytes (context-or-octets &rest options)
  "Construct a byte array constant, optionally taking a context before the octets."
  (let* ((explicit-context-p (context-p context-or-octets))
         (context (cl:if explicit-context-p context-or-octets (ensure-context)))
         (bytes (cl:if explicit-context-p (pop options) context-or-octets)))
    (destructuring-bind (&key nul-terminated) options
      (call-with-bytes bytes
        (lambda (data count) (%const-bytes context data count nul-terminated))))))

(defun const-array (element-type elements)
  "Construct an array constant from literals or matching LLVM constants."
  (let* ((resolved (resolve-type element-type))
         (coerced (mapcar (lambda (e) (const resolved e))
                          (sequence->list elements))))
    (call-with-handles coerced (lambda (data count) (%const-array resolved data count)))))

(defun const-struct (type fields)
  "Construct a struct constant, coercing each field to its declared type.
FIELDS accepts a proper list, vector, or treelist, including nested literals."
  (let ((resolved (resolve-type type)))
    (cl:unless (eq (type-kind resolved) :struct)
      (error 'llvm-error :operation 'const-struct :message "Expected an LLVM struct type."))
    (const resolved fields)))

(defun const-vector (type elements)
  "Construct a fixed vector with exactly the lanes specified by TYPE."
  (let ((resolved (resolve-type type)))
    (cl:unless (eq (type-kind resolved) :vector)
      (error 'llvm-error :operation 'const-vector :message "Expected a fixed LLVM vector type."))
    (let ((element-type (type-subtype resolved 0)))
      (call-with-handles (mapcar (lambda (value) (const element-type value)) (sequence->list elements))
        (lambda (data count) (%const-vector resolved data count))))))

(defun const-splat (type value)
  "Repeat a constant VALUE across a fixed or scalable vector of TYPE."
  (let ((resolved (resolve-type type)))
    (cl:unless (member (type-kind resolved) '(:vector :scalable-vector))
      (error 'llvm-error :operation 'const-splat :message "Expected an LLVM vector type."))
    (%const-splat resolved (const (type-subtype resolved 0) value))))

(defun gep-indices (indices context)
  (mapcar (lambda (index)
            (cl:if (value-p index)
                   index
                   (progn
                     (check-type index (signed-byte 32))
                     (const-int (int-type context 32) index))))
          (sequence->list indices)))

(defun const-gep (source-type address indices &key in-bounds)
  "Construct an address constant for a global initializer from a constant base and indices."
  (let* ((pointer (cl:or (operand-value address)
                         (error 'llvm-error :operation 'const-gep
                                :message "Expected a constant LLVM address.")))
         (context (type-context (value-type pointer)))
         (type (resolve-type source-type context)))
    (call-with-handles (gep-indices indices context)
      (lambda (data count) (%const-gep type pointer data count in-bounds)))))

(defun const (type-spec value &optional (context *context*))
  "Construct a constant of TYPE-SPEC from a Lisp literal or reuse a matching LLVM constant."
  (let* ((ty (resolve-type type-spec context))
         (operand (operand-value value)))
    (cl:cond
      (operand
       (cl:unless (value-constant-p operand)
         (error 'llvm-error :operation 'const :message "Expected an LLVM constant or Lisp literal."))
       (cl:unless (type-equal-p ty (value-type operand))
         (error 'llvm-error :operation 'const
                :message (format nil "Expected ~A, got ~A." (type-ir ty) (value-ir operand))))
       operand)
      ((eq value :zero) (const-zero ty))
      (t
       (ecase (type-kind ty)
         (:integer
          (let* ((integer (cl:cond ((eq value t) 1)
                                   ((null value) 0)
                                   ((integerp value) value)
                                   (t (error "Expected an integer or boolean for ~A, got ~S."
                                             (type-ir ty) value))))
                 (width (integer-width ty))
                 (bits (cl:if (minusp integer) (cl:1- width) width)))
            (cl:when (cl:> (integer-length integer) bits)
              (error 'llvm-error :operation 'const
                     :message (format nil "Integer ~D does not fit in LLVM type ~A. Use LLVM:CONST-INT for low bits."
                                      integer (type-ir ty))))
            (const-int ty integer)))
         ((:half :bfloat :float :double :x86-fp80 :fp128 :ppc-fp128)
          (const-float ty value))
         (:pointer
          (cl:if (cl:or (null value) (eql value 0))
                 (const-zero ty)
                 (error "Cannot coerce ~S to an LLVM pointer constant." value)))
         (:array
          (let* ((elements (sequence->list value))
                 (count (array-length ty)))
            (cl:unless (cl:= (length elements) count)
              (error 'llvm-error :operation 'const
                     :message (format nil "Expected ~D array elements, got ~D." count (length elements))))
            (const-array (type-subtype ty 0) elements)))
         (:vector (const-vector ty value))
         (:struct
          (let ((field-types (type-subtypes ty))
                (fields (sequence->list value)))
            (cl:unless (cl:= (length field-types) (length fields))
              (error 'llvm-error :operation 'const
                     :message (format nil "Expected ~D struct fields, got ~D."
                                      (length field-types) (length fields))))
            (call-with-handles (mapcar #'const field-types fields)
              (lambda (data count) (%const-struct ty data count))))))))))

(define-operation (%add-global add-global) "add_global" :global
    (module :handle) (name :cstring) (storage-type :handle) (initializer :optional-handle)
    (constant :bool) (linkage :uint32))
(define-operation (%add-global-in-address-space add-global) "add_global_in_address_space" :global
    (module :handle) (name :cstring) (storage-type :handle) (initializer :optional-handle)
    (constant :bool) (linkage :uint32) (address-space :uint32))
(define-operation find-global "find_global" :global (module :handle) (name :cstring))
(define-operation %first-global "first_global" :global (module :handle))
(define-operation %next-global "next_global" :global (global :handle))
(define-operation global-name "global_name" :text (global :handle))
(define-operation global-storage-type "global_storage_type" :type (global :handle))
(define-operation global-address "global_address" :value (global :handle))
(define-operation global-constant-p "global_is_constant" :bool (global :handle))
(define-operation global-initializer "global_initializer" :value (global :handle))
(define-operation set-global-initializer "set_global_initializer" :effect
    (global :handle) (value :handle))
(define-operation global-alignment "global_alignment" :uint64 (global :handle))
(define-operation set-global-alignment "set_global_alignment" :effect
    (global :handle) (alignment :uint64))
(define-operation (%global-linkage global-linkage) "global_linkage" :uint64 (global :handle))
(define-operation (%set-global-linkage set-global-linkage) "set_global_linkage" :effect
    (global :handle) (linkage :uint32))
(define-operation (%global-unnamed-address global-unnamed-address) "global_unnamed_address" :uint64
    (global :handle))
(define-operation (%set-global-unnamed-address set-global-unnamed-address) "set_global_unnamed_address" :effect
    (global :handle) (address :uint32))

(defparameter *linkages*
  '(:external :internal :private :available-externally :link-once :link-once-odr
    :weak :weak-odr :appending :external-weak :common))

(defparameter *unnamed-address-modes* '(:none :local :global))

(defun linkage-index (operation linkage)
  (operation-index operation linkage *linkages*))

(defun global-linkage (global)
  (nth (%global-linkage global) *linkages*))

(defun set-global-linkage (global linkage)
  (%set-global-linkage global (linkage-index 'set-global-linkage linkage)))

(defun global-unnamed-address (global)
  (nth (%global-unnamed-address global) *unnamed-address-modes*))

(defun set-global-unnamed-address (global mode)
  (%set-global-unnamed-address global
                               (operation-index 'set-global-unnamed-address mode *unnamed-address-modes*)))

(defun add-global (module name storage-type &key (initializer nil initializer-p)
                                                  constant (linkage :external) address-space)
  (let* ((ctx (module-context module))
         (resolved-type (resolve-type storage-type ctx))
         (init (cl:and initializer-p
                       (cl:or (operand-value initializer) (const resolved-type initializer ctx))))
         (linkage (linkage-index 'add-global linkage)))
    (cl:if address-space
           (%add-global-in-address-space module name resolved-type init constant linkage address-space)
           (%add-global module name resolved-type init constant linkage))))

(defun module-globals (&optional (module (ensure-module)))
  (cl:loop for global = (%first-global module) then (%next-global global)
        while global
        collect global))

(define-operation (%add-function add-function) "add_function_with_linkage" :function
    (module :handle) (name :cstring) (type :handle) (linkage :uint32))
(define-operation (%declare-function declare-function) "declare_function" :function
    (module :handle) (name :cstring) (result :handle) (parameters :pointer)
    (count :uintptr) (variadic :bool) (linkage :uint32))
(define-operation (%find-function find-function) "find_function" :function (module :handle) (name :cstring))
(define-operation (%declare-intrinsic declare-intrinsic) "declare_intrinsic" :function
    (module :handle) (base-name :cstring) (signature :handle))
(define-operation %first-function "first_function" :function (module :handle))
(define-operation %next-function "next_function" :function (function :handle))
(define-operation function-name "function_name" :text (function :handle))
(define-operation (%function-linkage function-linkage) "function_linkage" :uint64 (function :handle))
(define-operation (%set-function-linkage set-function-linkage) "set_function_linkage" :effect
    (function :handle) (linkage :uint32))
(define-operation (%function-module function-module) "function_module" :module (function :handle))
(define-operation (%function-signature function-signature) "function_signature" :type (function :handle))
(define-operation function-address "function_address" :value (function :handle))
(define-operation function-calling-convention "function_calling_convention_id" :uint64 (function :handle))
(define-operation (%set-function-calling-convention set-function-calling-convention)
    "set_function_calling_convention" :effect (function :handle) (convention :uint32))
(define-operation (%set-function-attribute set-function-attribute) "set_function_attribute" :effect
    (function :handle) (index :uint32) (name :cstring) (payload :uint32) (number :uint64)
    (type :optional-handle) (text :cstring))
(define-operation (%function-has-attribute-p function-has-attribute-p) "function_has_attribute" :bool
    (function :handle) (index :uint32) (name :cstring) (string :bool))
(define-operation (%remove-function-attribute remove-function-attribute) "remove_function_attribute" :effect
    (function :handle) (index :uint32) (name :cstring) (string :bool))
(define-operation parameter "parameter" :value (function :handle) (index :uint32))
(define-operation (%named-parameter define-function) "named_parameter" :value
    (function :handle) (index :uint32) (name :cstring))
(define-operation (%begin-function-body define-function) "begin_function_body" :builder
    (function :handle))
(define-operation (%begin-named-function-body define-function) "begin_named_function_body" :builder
    (module :handle) (name :cstring) (result :handle) (parameters :pointer)
    (count :uintptr) (variadic :bool) (linkage :uint32))
(define-operation %body-function "body_function" :function (builder :handle))
(define-operation (%finish-function-body define-function) "finish_function_body" :effect
    (builder :handle))
(define-operation (%append-block append-block) "append_block" :block
    (function :handle) (name :cstring))
(define-operation (%insert-block-before insert-block-before) "insert_block_before" :block
    (function :handle) (anchor :handle) (name :cstring))
(define-operation %first-block "first_block" :block (function :handle))
(define-operation block-next "next_block" :block (block :handle))
(define-operation block-previous "previous_block" :block (block :handle))
(define-operation block-name "block_name" :text (block :handle))
(define-operation block-function "block_function" :function (block :handle))
(define-operation (%block-terminated-p block-terminated-p) "block_is_terminated" :bool (block :handle))
(define-operation %builder-context "builder_context" :context (builder :handle))
(define-operation (%builder-block builder-block) "builder_block" :block (builder :handle))
(define-operation position-at-end "position_at_end" :effect
    (builder :handle) (block :handle))
(define-operation (%position-before position-before) "position_before_instruction" :effect
    (builder :handle) (instruction :handle))
(define-operation clear-position "clear_position" :effect (builder :handle))

(defun builder-block (builder)
  "Return a handle to BUILDER's insertion block, or NIL when unpositioned.
Compare block identity with BLOCK-EQUAL-P."
  (%builder-block builder))

(defgeneric operand-value (object)
  (:documentation "Return the IR value represented by OBJECT, or NIL for a Lisp literal."))

(defmethod operand-value ((object t))
  (declare (ignore object))
  nil)

(defmethod operand-value ((object value))
  object)

(defmethod operand-value ((object global))
  (global-address object))

(defmethod operand-value ((object ir-function))
  (function-address object))

(defun block-terminated-p (block)
  "Return true when BLOCK ends in an LLVM terminator. NIL denotes no block."
  (cl:and block (%block-terminated-p block)))

(defun function-module (function)
  (let ((owner (handle-owner function)))
    (cl:if (handle-open-p owner)
           owner
           (setf (handle-owner function) (%function-module function)))))

(defun function-context (function)
  (module-context (function-module function)))

(defun function-linkage (function)
  (nth (%function-linkage function) *linkages*))

(defun set-function-linkage (function linkage)
  (%set-function-linkage function (linkage-index 'set-function-linkage linkage)))

(defun function-signature (function)
  (remember-type-context (%function-signature function) (function-context function)))

(defun function-parameter-types (function)
  "Return the LLVM types of FUNCTION's fixed parameters in declaration order."
  (let ((signature (function-signature function)))
    (cl:loop for index from 1 below (type-subtype-count signature)
             collect (type-subtype signature index))))

(defun function-parameters (function)
  "Return FUNCTION's fixed parameters in declaration order."
  (cl:loop for index below (1- (type-subtype-count (function-signature function)))
           collect (parameter function index)))

(defun add-function (module name type &key (linkage :external))
  (let* ((ctx (module-context module))
         (fn (%add-function module name (resolve-type type ctx)
                            (linkage-index 'add-function linkage))))
    (setf (handle-owner fn) module)
    fn))

(defun find-function (module name)
  (let ((fn (%find-function module name)))
    (cl:when fn (setf (handle-owner fn) module))
    fn))

(defun declare-intrinsic (module base-name signature)
  (let* ((ctx (module-context module))
         (fn (%declare-intrinsic module base-name (resolve-type signature ctx))))
    (setf (handle-owner fn) module)
    fn))

(defun declare-function (module name result parameters &key variadic linkage)
  (let* ((ctx (module-context module))
         (res (resolve-type result ctx))
         (params (mapcar (lambda (p) (resolve-type p ctx)) (sequence->list parameters)))
         (link (cl:if linkage (linkage-index 'declare-function linkage) #xffffffff))
         (fn (call-with-handles params
               (lambda (data count)
                 (%declare-function module name res data count variadic link)))))
    (setf (handle-owner fn) module)
    fn))

(defun position-before (builder instruction)
  "Position BUILDER before INSTRUCTION, including a block's terminator."
  (%position-before builder instruction))

(defun call-with-function-builder (builder names emit &key function module)
  (unwind-protect
       (let* ((function (cl:or function (%body-function builder)))
              (*module* (cl:or module (function-module function)))
              (*context* (module-context *module*))
              (*function* function)
              (*builder* builder))
         (declare (special *function* *builder*))
         (setf (handle-owner function) *module*
               (handle-owner builder) *context*)
         (apply emit function builder
                (cl:loop for name in names for index from 0
                         collect (%named-parameter function index name)))
         (%finish-function-body builder)
         function)
    (release builder)))

(defun call-with-named-function-builder (module name result parameters variadic linkage names emit)
  (let* ((context (module-context module))
         (result (resolve-type result context))
         (parameters (mapcar (lambda (type) (resolve-type type context)) parameters))
         (linkage (cl:if linkage (linkage-index 'define-function linkage) #xffffffff))
         (builder (call-with-handles parameters
                    (lambda (data count)
                      (%begin-named-function-body module name result data count variadic linkage)))))
    (call-with-function-builder builder names emit :module module)))

(defun emit-function-body (function parameter-names emitter)
  "Build the body of FUNCTION, calling EMITTER with a fresh list of its LLVM parameters.
PARAMETER-NAMES supplies one string per fixed parameter. EMITTER emits the entire body,
including its terminators, with *MODULE*, *FUNCTION*, *BUILDER*, and *CONTEXT* bound.
An unsuccessful emission discards the body and restores the declaration.
Handles to discarded IR signal LLVM-ERROR when used.
Return FUNCTION after releasing the builder."
  (let* ((names (sequence->list parameter-names))
         (emit (etypecase emitter
                 (function emitter)
                 (symbol (symbol-function emitter))))
         (arity (1- (type-subtype-count (function-signature function)))))
    (cl:unless (cl:= (length names) arity)
      (error "LLVM function ~A needs ~D parameter names, got ~D."
             (function-name function) arity (length names)))
    (dolist (name names)
      (check-type name string)
      (cl:when (find (code-char 0) name)
        (error "LLVM parameter name ~S contains a NUL character." name)))
    (call-with-function-builder (%begin-function-body function) names
      (lambda (fn builder &rest parameters)
        (declare (ignore fn builder))
        (funcall emit parameters))
      :function function)))

(defmacro define-function ((function builder module name result &key variadic linkage)
                           parameters &body body)
  "Run BODY with a builder for the named function and return the completed function.
An unsuccessful emission discards the body and allows another attempt.
A newly created symbol survives a failed attempt as an external declaration."
  (let ((variables nil)
        (types nil))
    (dolist (parameter parameters)
      (destructuring-bind (variable type) parameter
        (push variable variables)
        (push type types)))
    (setf variables (nreverse variables)
          types (nreverse types))
    `(call-with-named-function-builder
      ,module ,name ,result (list ,@types) ,variadic ,linkage
      ',(mapcar (lambda (variable) (string-downcase (symbol-name variable))) variables)
      (lambda (,function ,builder ,@variables)
        (declare (ignorable ,function ,builder ,@variables))
        ,@body))))

(defun calling-convention-id (convention)
  (case convention (:c 0) (:fast 8) (:cold 9) (otherwise convention)))

(defun set-function-calling-convention (function convention)
  (%set-function-calling-convention function (calling-convention-id convention)))

(defun attribute-location-index (location)
  (etypecase location
    (keyword (ecase location (:function #xffffffff) (:return 0)))
    ((integer 0 #xfffffffd) (cl:1+ location))))

(defun set-ir-attribute (setter owner context location name value)
  (let ((index (attribute-location-index location)))
    (cl:if (cl:and (consp value) (eq (first value) :range))
           (destructuring-bind (kind width lower upper) value
             (declare (ignore kind))
             (check-type width (integer 1 #xffffffff))
             (check-type lower integer)
             (check-type upper integer)
             (funcall setter owner index name 4 width nil (format nil "~D,~D" lower upper)))
           (let ((resolved-value (cl:if (cl:or (keywordp value) (consp value))
                                        (resolve-type value (funcall context owner))
                                        value)))
             (etypecase resolved-value
               (null (funcall setter owner index name 0 0 nil ""))
               ((unsigned-byte 64) (funcall setter owner index name 1 resolved-value nil ""))
               (handle (funcall setter owner index name 2 0 resolved-value ""))
               (string (funcall setter owner index name 3 0 nil resolved-value)))))))

(defun set-function-attribute (function location name &optional value)
  "Set a function, return, or parameter attribute. For \"range\", VALUE is
(:RANGE BIT-WIDTH LOWER UPPER), with signed or unsigned integer bounds."
  (set-ir-attribute #'%set-function-attribute function #'function-context location name value))

(defun function-has-attribute-p (function location name &key string)
  "Look up NAME at LOCATION. STRING selects the string key if NAME is also built-in."
  (%function-has-attribute-p function (attribute-location-index location) name string))

(defun remove-function-attribute (function location name &key string)
  "Remove the attribute selected by FUNCTION-HAS-ATTRIBUTE-P."
  (%remove-function-attribute function (attribute-location-index location) name string))

(defun append-block (function &optional (name ""))
  (%append-block function name))

(defun insert-block-before (function anchor &optional (name ""))
  "Create a block in FUNCTION immediately before ANCHOR. Inserting first changes the entry block."
  (%insert-block-before function anchor name))

(defun module-functions (&optional (module (ensure-module)))
  (cl:loop for function = (%first-function module) then (%next-function function)
        while function
        do (setf (handle-owner function) module)
        collect function))

(defun function-blocks (&optional (function *function*))
  (cl:loop for block = (%first-block function) then (block-next block)
        while block
        collect block))

(defun find-block (function name)
  "Find a block by its exact LLVM name, or return NIL."
  (check-type name string)
  (cl:loop for block = (%first-block function) then (block-next block)
           while block
           when (equal name (block-name block)) return block))

(define-operation (%int-binop build-int-binop) "int_binop_flags" :value
    (builder :handle) (operation :uint32) (left :handle) (right :handle)
    (nsw :bool) (nuw :bool) (exact :bool) (name :cstring))
(define-operation (%float-binop build-float-binop) "float_binop_with_flags" :value
    (builder :handle) (operation :uint32) (left :handle) (right :handle)
    (flags :uint32) (name :cstring))
(define-operation (%fneg build-fneg) "fneg_with_flags" :value
    (builder :handle) (value :handle) (flags :uint32) (name :cstring))
(define-operation (%icmp build-icmp) "icmp" :value
    (builder :handle) (predicate :uint32) (left :handle) (right :handle) (name :cstring))
(define-operation (%fcmp build-fcmp) "fcmp_with_flags" :value
    (builder :handle) (predicate :uint32) (left :handle) (right :handle)
    (flags :uint32) (name :cstring))
(define-operation (%int-cast build-int-cast) "int_cast" :value
    (builder :handle) (value :handle) (type :handle) (signed :bool) (name :cstring))
(define-operation (%cast build-cast) "cast" :value
    (builder :handle) (operation :uint32) (value :handle) (type :handle) (name :cstring))

(defun builder-context (builder)
  (let ((context (handle-owner builder)))
    (cl:if (handle-open-p context)
           context
           (setf (handle-owner builder) (%builder-context builder)))))

(defun instruction-name-options (options)
  (cl:if (cl:and options (cl:not (keywordp (first options))))
         (list* :name (first options) (rest options))
         options))

(defparameter *int-predicates* '(:eq :ne :ugt :uge :ult :ule :sgt :sge :slt :sle))
(defparameter *float-predicates*
  '(:oeq :ogt :oge :olt :ole :one :ord :uno :ueq :ugt :uge :ult :ule :une :false :true))

(defun build-int-binop (builder operation left right &rest options)
  "Emit an integer operation. Accept :NSW, :NUW, or :EXACT where LLVM permits them."
  (destructuring-bind (&key (name "") nsw nuw exact) (instruction-name-options options)
    (%int-binop builder (operation-index 'build-int-binop operation
                                        '(:add :sub :mul :udiv :sdiv :urem :srem
                                          :and :or :xor :shl :lshr :ashr))
                left right nsw nuw exact name)))

(defun build-add (builder left right &rest options)
  (apply #'build-int-binop builder :add left right options))

(defun build-sub (builder left right &rest options)
  (apply #'build-int-binop builder :sub left right options))

(defun build-mul (builder left right &rest options)
  (apply #'build-int-binop builder :mul left right options))

(defparameter *fast-math-flags* '(:reassoc :nnan :ninf :nsz :arcp :contract :afn))
(defconstant +all-fast-math-flags+ #x7f)

(defun fast-math-flags-mask (operation flags)
  (let ((mask 0))
    (dolist (flag (cl:if (keywordp flags) (list flags) flags) mask)
      (setf mask (logior mask
                         (cl:if (eq flag :fast)
                                +all-fast-math-flags+
                                (ash 1 (operation-index operation flag *fast-math-flags*))))))))

(defun build-float-binop (builder operation left right &rest options)
  "Emit floating-point arithmetic. :FAST-MATH accepts a flag or a list of flags.
With flags, emit an instruction even when both operands are constant."
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (%float-binop builder (operation-index 'build-float-binop operation '(:add :sub :mul :div :rem))
                  left right (fast-math-flags-mask 'build-float-binop fast-math) name)))

(defun build-fneg (builder value &rest options)
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (%fneg builder value (fast-math-flags-mask 'build-fneg fast-math) name)))

(defun build-icmp (builder predicate left right &optional (name ""))
  (%icmp builder (operation-index 'build-icmp predicate *int-predicates*)
         left right name))

(defun build-fcmp (builder predicate left right &rest options)
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (%fcmp builder (operation-index 'build-fcmp predicate *float-predicates*)
           left right (fast-math-flags-mask 'build-fcmp fast-math) name)))

(defun build-int-cast (builder value type &key signed (name ""))
  (%int-cast builder value (resolve-type type (builder-context builder)) signed name))

(defun build-cast (builder operation value type &optional (name ""))
  (%cast builder (operation-index 'build-cast operation
                                 '(:trunc :zext :sext :fptoui :fptosi :uitofp :sitofp
                                   :fptrunc :fpext :ptrtoint :inttoptr :bitcast :addrspacecast))
         value (resolve-type type (builder-context builder)) name))

(define-operation (%phi build-phi) "phi" :value (builder :handle) (type :handle) (name :cstring))
(define-operation add-incoming "add_incoming" :effect (phi :handle) (value :handle) (block :handle))
(define-operation (%ret build-ret) "ret" :effect (builder :handle) (value :optional-handle))
(define-operation build-br "br" :effect (builder :handle) (block :handle))
(define-operation build-cond-br "cond_br" :effect
    (builder :handle) (test :handle) (yes :handle) (no :handle))
(define-operation (%select build-select) "select" :value
    (builder :handle) (test :handle) (yes :handle) (no :handle) (name :cstring))
(define-operation (%switch build-switch) "switch" :effect
    (builder :handle) (value :handle) (default :handle) (cases :pointer) (count :uintptr))
(define-operation build-unreachable "unreachable" :effect (builder :handle))
(define-operation %instruction-context "instruction_context" :context (instruction :handle))
(define-operation (%tail-call-kind tail-call-kind) "instruction_tail_call_kind" :uint64 (call :handle))
(define-operation (%set-tail-call-kind set-tail-call-kind) "set_instruction_tail_call_kind" :effect
    (call :handle) (kind :uint32))
(define-operation call-calling-convention "call_calling_convention_id" :uint64 (call :handle))
(define-operation (%set-call-calling-convention set-call-calling-convention)
    "set_call_calling_convention" :effect (call :handle) (convention :uint32))
(define-operation (%set-call-attribute set-call-attribute) "set_call_attribute" :effect
    (call :handle) (index :uint32) (name :cstring) (payload :uint32) (number :uint64)
    (type :optional-handle) (text :cstring))
(define-operation (%call-has-attribute-p call-has-attribute-p) "call_has_attribute" :bool
    (call :handle) (index :uint32) (name :cstring) (string :bool))
(define-operation (%remove-call-attribute remove-call-attribute) "remove_call_attribute" :effect
    (call :handle) (index :uint32) (name :cstring) (string :bool))

(defparameter *tail-call-kinds* '(:none :tail :musttail :notail))

(defun tail-call-kind (call)
  (nth (%tail-call-kind call) *tail-call-kinds*))

(defun set-tail-call-kind (call kind)
  (%set-tail-call-kind call (operation-index 'set-tail-call-kind kind *tail-call-kinds*)))

(defun set-call-calling-convention (call convention)
  (%set-call-calling-convention call (calling-convention-id convention)))

(defun set-call-attribute (call location name &optional value)
  "Set an attribute on CALL at :FUNCTION, :RETURN, or a zero-based argument index.
For \"range\", VALUE is (:RANGE BIT-WIDTH LOWER UPPER)."
  (set-ir-attribute #'%set-call-attribute call #'%instruction-context location name value))

(defun call-has-attribute-p (call location name &key string)
  "Check for an attribute explicitly attached to CALL. STRING can select a string key."
  (%call-has-attribute-p call (attribute-location-index location) name string))

(defun remove-call-attribute (call location name &key string)
  "Remove the attribute selected by CALL-HAS-ATTRIBUTE-P."
  (%remove-call-attribute call (attribute-location-index location) name string))

(defun build-phi (builder type &optional (name ""))
  (%phi builder (resolve-type type (builder-context builder)) name))

(defun build-ret (builder &optional value)
  (%ret builder value))

(defun build-select (builder test yes no &optional (name ""))
  (%select builder test yes no name))

(defun build-switch (builder value default cases)
  (let ((case-list (sequence->list cases)))
    (call-with-handles (cl:loop for case in case-list
                             append (destructuring-bind (key block) case (list key block)))
      (lambda (data count) (%switch builder value default data count)))))

(defun emit-indirect-call (operation builder signature callee arguments name)
  (let ((sig (cl:if (handle-p signature)
                    signature
                    (resolve-type signature (builder-context builder)))))
    (call-with-handles arguments
      (lambda (data count) (funcall operation builder sig callee data count name)))))

(define-operation (%extract-value build-extract-value) "extract_value" :value
    (builder :handle) (aggregate :handle) (index :uint32) (name :cstring))
(define-operation (%insert-value build-insert-value) "insert_value" :value
    (builder :handle) (aggregate :handle) (value :handle) (index :uint32) (name :cstring))
(define-operation (%extract-element build-extract-element) "extract_element" :value
    (builder :handle) (vector :handle) (index :handle) (name :cstring))
(define-operation (%insert-element build-insert-element) "insert_element" :value
    (builder :handle) (vector :handle) (value :handle) (index :handle) (name :cstring))
(define-operation (%shuffle-vector build-shuffle-vector) "shuffle_vector" :value
    (builder :handle) (left :handle) (right :handle) (mask :handle) (name :cstring))
(define-operation (%freeze build-freeze) "freeze" :value
    (builder :handle) (value :handle) (name :cstring))
(define-operation (%not build-not) "not" :value
    (builder :handle) (value :handle) (name :cstring))

(defun build-extract-value (builder aggregate index &optional (name ""))
  (%extract-value builder aggregate index name))

(defun build-insert-value (builder aggregate value index &optional (name ""))
  (%insert-value builder aggregate value index name))

(defun build-extract-element (builder vector index &optional (name ""))
  (%extract-element builder vector index name))

(defun build-insert-element (builder vector value index &optional (name ""))
  (%insert-element builder vector value index name))

(defun build-shuffle-vector (builder left right mask &optional (name ""))
  (%shuffle-vector builder left right mask name))

(defun build-freeze (builder value &optional (name ""))
  (%freeze builder value name))

(defun build-not (builder value &optional (name ""))
  (%not builder value name))

(defmethod print-object ((object handle) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:unless (handle-alive-p object)
      (write-string (handle-status-label object) stream))))

(defmethod print-object ((object ir-type) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (cl:if (handle-alive-p object) (type-ir object) ":RELEASED") stream)))

(defmethod print-object ((object value) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (cl:if (handle-alive-p object)
                         (string-trim '(#\Space #\Tab #\Newline) (value-ir object))
                         (handle-status-label object))
                  stream)))

(defmethod print-object ((object ir-function) stream)
  (print-unreadable-object (object stream :type t)
    (cl:if (handle-alive-p object)
           (format stream "@~A ~A" (function-name object) (type-ir (function-signature object)))
           (write-string ":RELEASED" stream))))

(defmethod print-object ((object global) stream)
  (print-unreadable-object (object stream :type t)
    (cl:if (handle-alive-p object)
           (format stream "@~A ~A" (global-name object) (type-ir (global-storage-type object)))
           (write-string ":RELEASED" stream))))

(defmethod print-object ((object ir-block) stream)
  (print-unreadable-object (object stream :type t)
    (cl:if (handle-alive-p object)
           (format stream "~A" (block-name object))
           (write-string (handle-status-label object) stream))))
