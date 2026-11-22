(in-package #:zk-llvm)

(define-operation (%make-jit make-jit) "make_jit" :jit)
(define-operation (%make-jit-dylib make-jit-dylib) "make_jit_dylib" :jit
    (parent :handle) (name :cstring))
(define-operation (%make-resource-tracker make-resource-tracker) "make_resource_tracker"
    :resource-tracker (jit :handle))
(define-operation (%remove-resource-tracker remove-resource-tracker) "remove_resource_tracker"
    :effect (tracker :handle))
(define-operation (%jit-add-module jit-add-module) "jit_add_module" :effect
    (jit :handle) (module :handle))
(define-operation (%jit-add-module-with-tracker jit-add-module) "jit_add_module_with_tracker" :effect
    (jit :handle) (module :handle) (tracker :handle))
(define-operation (%jit-add-ir-module jit-add-ir-module) "jit_add_ir_module" :effect
    (jit :handle) (module :handle) (tracker :optional-handle))
(define-operation (%jit-add-object jit-add-object) "jit_add_object" :effect
    (jit :handle) (data :pointer) (count :uintptr))
(define-operation (%jit-add-object-with-tracker jit-add-object) "jit_add_object_with_tracker" :effect
    (jit :handle) (data :pointer) (count :uintptr) (tracker :handle))
(define-operation (%jit-define-symbol jit-define-symbol) "jit_define_symbol" :effect
    (jit :handle) (name :cstring) (address :pointer) (kind :uint32))
(define-operation jit-triple "jit_triple" :text (jit :handle))
(define-operation jit-data-layout "jit_data_layout" :text (jit :handle))
(define-operation jit-configure-module "jit_configure_module" :effect (jit :handle) (module :handle))
(define-operation (%jit-lookup jit-lookup) "jit_lookup" :jit-symbol (jit :handle) (name :cstring))
(define-operation (%jit-add-module-and-lookup jit-fn) "jit_add_module_and_lookup" :jit-symbol
    (jit :handle) (module :handle) (name :cstring))
(define-operation (%jit-remove-compilation release) "jit_remove_compilation" :optional-text
    (symbol :handle))
(define-operation (%jit-symbol-address jit-symbol-address) "jit_symbol_address" :pointer (symbol :handle))
(define-operation (%close-jit close-jit) "close_jit" :effect (jit :handle))

(defstruct (jit-imports (:constructor make-jit-imports (parent)) (:copier nil))
  (parent nil :read-only t)
  (pointers (tl:mutable-treelist) :read-only t))

(defun make-jit ()
  (let ((jit (%make-jit)))
    (setf (handle-owner jit) (make-jit-imports nil))
    jit))

(defun make-jit-dylib (parent name)
  (let* ((imports (make-jit-imports (handle-owner parent)))
         (jit (%make-jit-dylib parent name)))
    (setf (handle-owner jit) imports)
    jit))

(defun make-resource-tracker (jit)
  "Create a group of submissions in JIT. Releasing the handle keeps its code in JIT.
Use REMOVE-RESOURCE-TRACKER to unload the group explicitly."
  (let ((tracker (%make-resource-tracker jit)))
    (setf (handle-owner tracker) (handle-owner jit))
    tracker))

(defun remove-resource-tracker (tracker)
  "Unload TRACKER's submissions and free their names for reuse in the same JITDylib.
Release live lookup symbols and remove dependent code before unloading its imports.
A removed tracker cannot accept new submissions. Repeated removal has no effect."
  (%remove-resource-tracker tracker)
  (setf (handle-owner tracker) nil))

(defun jit-add-module (jit module &key tracker)
  "Compile a verified snapshot of MODULE and submit its object to JIT."
  (cl:if tracker
         (%jit-add-module-with-tracker jit module tracker)
         (%jit-add-module jit module)))

(defun jit-add-ir-module (jit module &key tracker)
  "Submit verified IR for ORC to compile on demand. TRACKER must belong to JIT."
  (%jit-add-ir-module jit module tracker))

(defun jit-lookup (jit name)
  (let* ((imports (handle-owner jit))
         (symbol (%jit-lookup jit name)))
    (setf (handle-owner symbol) imports)
    symbol))

(defun close-jit (jit)
  "End submission and lookup. Existing symbols and children retain their imports."
  (%close-jit jit)
  (setf (handle-owner jit) nil))

(defun jit-define-symbol (jit name address &key (kind :function))
  "Define an unmangled name at a foreign ADDRESS of :FUNCTION or :DATA KIND.
The JIT retains ADDRESS together with the code that can reference it."
  (let* ((index (operation-index 'jit-define-symbol kind '(:function :data)))
         (imports (handle-owner jit)))
    (%jit-define-symbol jit name address index)
    (tl:push-back! (jit-imports-pointers imports) address)
    nil))

(defun jit-add-object (jit octets &key tracker)
  "Link a trusted relocatable host object. TRACKER, when supplied, must belong to JIT."
  (call-with-bytes octets
    (lambda (data count)
      (cl:if tracker
             (%jit-add-object-with-tracker jit data count tracker)
             (%jit-add-object jit data count)))))

(defun jit-symbol-address (symbol)
  "Borrow SYMBOL's address. Retain SYMBOL for every use of the address."
  (%jit-symbol-address symbol))

(defun normalize-ffi-type (type)
  (case type
    (:i8 :int8)
    ((:u8 :bool :i1) :uint8)
    (:i16 :int16)
    (:u16 :uint16)
    (:i32 :int32)
    (:u32 :uint32)
    (:i64 :int64)
    (:u64 :uint64)
    (:f32 :float)
    (:f64 :double)
    (:ptr :pointer)
    (otherwise type)))

(defun bind-jit-function (symbol result-type &rest parameter-types)
  "Bind a JIT symbol using its C ABI. Accepts both FFI types (:INT32) and IR types (:I32).
Use :I32 for a signed C integer and :U32 for an unsigned one. LLVM IR uses i32 for both.
The returned closure keeps SYMBOL reachable and requires it to remain unreleased."
  (let ((binding (apply #'ffi:bind-pointer (jit-symbol-address symbol)
                        (normalize-ffi-type result-type)
                        (mapcar #'normalize-ffi-type parameter-types))))
    (lambda (&rest arguments)
      (let ((*foreign-arguments* (cons symbol *foreign-arguments*)))
        (handle-pointer symbol)
        (apply binding arguments)))))

(defun release-jit-compilation (symbol)
  (cl:when (handle-open-p symbol)
    (ensure-handle-releasable symbol)
    (let ((cleanup-error (%jit-remove-compilation symbol)))
      (release-handle symbol)
      (cl:when cleanup-error
        (error 'llvm-error :operation 'release :message cleanup-error)))))

(defun bind-owned-jit-symbol (symbol result-type parameter-types &optional (release-symbol #'release))
  (let ((closure nil))
    (unwind-protect
         (values (setf closure (apply #'bind-jit-function symbol result-type parameter-types))
                 (lambda () (funcall release-symbol symbol)))
      (cl:unless closure (funcall release-symbol symbol)))))

(defun jit-bind (jit name result-type &rest parameter-types)
  "Return a callable and a zero-argument function that releases its lookup symbol.
Supply the function's C ABI types, including integer signedness. The release
function is idempotent and invalidates the callable."
  (let ((symbol (let ((*object-scope* nil))
                  (jit-lookup jit (c-symbol-name name)))))
    (bind-owned-jit-symbol symbol result-type parameter-types)))

(defmacro with-jit-binding ((var jit name result-type &rest parameter-types) &body body)
  "Bind VAR to NAME using the supplied C ABI. Release the binding when BODY exits."
  (let ((release (gensym "RELEASE")))
    `(multiple-value-bind (,var ,release)
         (jit-bind ,jit ,name ,result-type ,@parameter-types)
       (declare (ignorable ,var))
       (unwind-protect (locally ,@body)
         (funcall ,release)))))

(defmacro with-jit-module ((jit module &key on-demand) &body body)
  "Submit MODULE to JIT and unload it when BODY exits.
With :ON-DEMAND, submit IR and let ORC compile it when a symbol is needed.
Release all bindings to the module and unload dependent code before leaving BODY."
  (let ((engine (gensym "JIT"))
        (tracker (gensym "TRACKER")))
    `(let* ((,engine ,jit)
            (,tracker (make-resource-tracker ,engine)))
       (unwind-protect
            (progn
              (funcall (cl:if ,on-demand #'jit-add-ir-module #'jit-add-module)
                       ,engine ,module :tracker ,tracker)
              (locally ,@body))
         (unwind-protect (remove-resource-tracker ,tracker)
           (release ,tracker))))))

(defmacro with-jit ((var &optional (jit-form '(make-jit))) &body body)
  "Bind VAR to an ORC JIT handle and close it when BODY exits."
  (let ((jit (gensym "JIT")))
    `(let ((,jit ,jit-form))
       (unwind-protect
            (let ((,var ,jit))
              (declare (ignorable ,var))
              ,@body)
         (cl:when (handle-open-p ,jit)
           (close-jit ,jit))))))

(defstruct (jit-abi-type (:constructor make-jit-abi-type (ir ffi &optional extension lisp-type)))
  ir ffi extension lisp-type)

(defun resolve-jit-abi-type (spec &optional result-p)
  (let* ((ir (resolve-type spec))
         (ffi (case (type-kind ir)
                (:integer
                 (let* ((width (integer-width ir))
                        (unsigned (cl:or (cl:= width 1) (member spec '(:u8 :u16 :u32 :u64)))))
                   (case width
                     ((1 8 16)
                      (return-from resolve-jit-abi-type
                        (make-jit-abi-type ir (cl:if unsigned :uint32 :int32)
                                           (cl:if unsigned :zext :sext)
                                           (list (cl:if unsigned 'unsigned-byte 'signed-byte) width))))
                     (32 (cl:if unsigned :uint32 :int32))
                     (64 (cl:if unsigned :uint64 :int64)))))
                (:float :float)
                (:double :double)
                (:pointer (cl:when (zerop (pointer-address-space ir)) :pointer))
                (:void (cl:when result-p :void)))))
    (cl:unless ffi
      (error 'llvm-error :operation 'jit-fn
             :message (format nil "No automatic C ABI for ~S. Use DEFINE-MODULE and an explicit JIT-BIND signature."
                              spec)))
    (make-jit-abi-type ir ffi)))

(defun jit-entry-name (module function result parameters)
  (let ((name (function-name function)))
    (cl:unless (cl:or (jit-abi-type-extension result) (some #'jit-abi-type-extension parameters))
      (return-from jit-entry-name name))
    ;; libffi and LLVM agree on a full-width C integer at the call boundary.
    ;; Adapt narrow IR types inside the generated module.
    (let* ((entry (format nil "~A.~A" name (gensym "ZKCL_ABI_")))
           (i32 (int-type (module-context module) 32))
           (boundary-types
             (lambda (type) (cl:if (jit-abi-type-extension type) i32 (jit-abi-type-ir type))))
           (bridge (declare-function module entry (funcall boundary-types result)
                                     (mapcar boundary-types parameters))))
      (emit-function-body
       bridge (cl:loop for index from 0 below (length parameters) collect (format nil "arg~D" index))
       (lambda (values)
         (let* ((arguments (cl:loop for value in values
                                 for type in parameters
                                 collect (cl:if (jit-abi-type-extension type)
                                                (build-cast *builder* :trunc value (jit-abi-type-ir type))
                                                value)))
                (value (build-call *builder* function arguments))
                (extension (jit-abi-type-extension result)))
           (build-ret *builder*
                      (cl:if extension (build-cast *builder* extension value i32) value)))))
      entry)))

(defun bind-jit-entry (jit module name result parameters)
  (let* ((parameter-types (mapcar #'jit-abi-type-ffi parameters))
         (types (mapcar #'jit-abi-type-lisp-type parameters))
         (symbol (let ((*object-scope* nil))
                   (%jit-add-module-and-lookup jit module name))))
    (setf (handle-owner symbol) (handle-owner jit))
    (multiple-value-bind (closure release)
        (bind-owned-jit-symbol symbol (jit-abi-type-ffi result) parameter-types
                               #'release-jit-compilation)
      (values (cl:if (some #'identity types)
                     (lambda (&rest arguments)
                       (cl:loop for argument in arguments for type in types
                             do (cl:when (cl:and type (cl:not (typep argument type)))
                                  (error 'type-error :datum argument :expected-type type)))
                       (apply closure arguments))
                     closure)
              release))))

(defun call-with-jit-compilation (existing emit)
  (with-objects ()
    (let ((jit (cl:or existing (make-jit))))
      (unwind-protect
           (funcall emit jit)
        (cl:unless existing (close-jit jit))))))
