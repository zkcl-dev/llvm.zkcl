(in-package #:zk-llvm)

(define-operation (%alloca build-alloca) "alloca_options" :value
                  (builder :handle) (type :handle) (count :optional-handle) (alignment :uint64) (name :cstring))
(define-operation (%load build-load) "load_options" :value
                  (builder :handle) (type :handle) (pointer :handle) (alignment :uint64) (volatile :bool)
                  (ordering :uint32) (scope :uint32) (name :cstring))
(define-operation (%store build-store) "store_options" :effect
                  (builder :handle) (value :handle) (pointer :handle) (alignment :uint64) (volatile :bool)
                  (ordering :uint32) (scope :uint32))
(define-operation (%gep build-gep) "gep_flags" :value
                  (builder :handle) (type :handle) (pointer :handle) (indices :pointer) (count :uintptr)
                  (in-bounds :bool) (nuw :bool) (nusw :bool) (name :cstring))
(define-operation (%fence build-fence) "fence" :effect
                  (builder :handle) (ordering :uint32) (scope :uint32))
(define-operation (%atomic-rmw build-atomic-rmw) "atomic_rmw" :value
                  (builder :handle) (operation :uint32) (pointer :handle) (value :handle) (ordering :uint32)
                  (scope :uint32) (alignment :uint64) (volatile :bool) (name :cstring))
(define-operation (%cmpxchg build-cmpxchg) "cmpxchg" :value
                  (builder :handle) (pointer :handle) (expected :handle) (replacement :handle)
                  (success-ordering :uint32) (failure-ordering :uint32) (scope :uint32) (alignment :uint64) (volatile :bool)
                  (weak :bool) (name :cstring))
(define-operation (%memcpy build-memcpy) "memcpy" :instruction
                  (builder :handle) (destination :handle) (source :handle) (length :handle)
                  (destination-alignment :uint64) (source-alignment :uint64) (volatile :bool))
(define-operation (%memmove build-memmove) "memmove" :instruction
                  (builder :handle) (destination :handle) (source :handle) (length :handle)
                  (destination-alignment :uint64) (source-alignment :uint64) (volatile :bool))
(define-operation (%memset build-memset) "memset" :instruction
                  (builder :handle) (destination :handle) (byte :handle) (length :handle)
                  (destination-alignment :uint64) (volatile :bool))
(define-operation (%malloc build-malloc) "malloc" :value
                  (builder :handle) (allocator :handle) (type :handle) (count :handle) (name :cstring))
(define-operation (%free build-free) "free" :instruction
                  (builder :handle) (deallocator :handle) (pointer :handle))
(define-operation (%ptr-diff build-ptr-diff) "ptr_diff" :value
                  (builder :handle) (type :handle) (left :handle) (right :handle) (name :cstring))
(define-operation (%add-global-string add-global-string) "add_global_string" :global
                  (module :handle) (name :cstring) (data :pointer) (length :uintptr) (nul-terminated :bool))

(define-operation (%instruction-memory-ordering instruction-memory-ordering)
    "instruction_memory_ordering" :uint64 (instruction :handle))
(define-operation (%set-instruction-memory-ordering set-instruction-memory-ordering)
    "set_instruction_memory_ordering" :effect (instruction :handle) (ordering :uint32))
(define-operation (%instruction-atomic-sync-scope instruction-atomic-sync-scope)
    "instruction_atomic_sync_scope" :uint64 (instruction :handle))
(define-operation (%set-instruction-atomic-sync-scope set-instruction-atomic-sync-scope)
    "set_instruction_atomic_sync_scope" :effect (instruction :handle) (scope :uint32))
(define-operation (%instruction-cmpxchg-success-ordering instruction-cmpxchg-success-ordering)
    "instruction_cmpxchg_success_ordering" :uint64 (instruction :handle))
(define-operation (%instruction-cmpxchg-failure-ordering instruction-cmpxchg-failure-ordering)
    "instruction_cmpxchg_failure_ordering" :uint64 (instruction :handle))
(define-operation (%set-instruction-cmpxchg-orderings set-instruction-cmpxchg-orderings)
    "set_instruction_cmpxchg_orderings" :effect (instruction :handle) (success :uint32) (failure :uint32))
(define-operation instruction-cmpxchg-weak-p "instruction_cmpxchg_is_weak" :bool (instruction :handle))
(define-operation set-instruction-cmpxchg-weak "set_instruction_cmpxchg_weak" :effect
    (instruction :handle) (weak :bool))
(define-operation (%instruction-atomic-rmw-operation instruction-atomic-rmw-operation)
    "instruction_atomic_rmw_operation" :uint64 (instruction :handle))
(define-operation (%set-instruction-atomic-rmw-operation set-instruction-atomic-rmw-operation)
    "set_instruction_atomic_rmw_operation" :effect (instruction :handle) (operation :uint32))
(define-operation set-instruction-volatile "set_instruction_volatile" :effect
    (instruction :handle) (volatile :bool))

(defparameter *atomic-orderings* '(:unordered :monotonic :acquire :release :acquire-release :seq-cst))
(defparameter *atomic-rmw-operations*
  '(:xchg :add :sub :and :nand :or :xor :max :min :umax :umin
    :fadd :fsub :fmax :fmin :uinc-wrap :udec-wrap :usub-cond :usub-sat :fmaximum :fminimum))

(defun atomic-ordering-index (operation ordering)
  (cl:1+ (operation-index operation
                          (case ordering (:acq-rel :acquire-release) (otherwise ordering))
                          *atomic-orderings*)))

(defun atomic-ordering-name (index)
  (cl:when (plusp index)
    (nth (cl:1- index) *atomic-orderings*)))

(defun sync-scope-index (operation scope)
  (operation-index operation scope '(:system :single-thread)))

(defun instruction-memory-ordering (instruction)
  "Return the ordering keyword of an atomic load, store, fence, or atomicrmw, or NIL."
  (atomic-ordering-name (%instruction-memory-ordering instruction)))

(defun set-instruction-memory-ordering (instruction ordering)
  "Set the ordering of a load, store, fence, or atomicrmw. NIL makes a load or store non-atomic."
  (%set-instruction-memory-ordering
   instruction (cl:if ordering (atomic-ordering-index 'set-instruction-memory-ordering ordering) 0)))

(defun instruction-atomic-sync-scope (instruction)
  "Return :SYSTEM, :SINGLE-THREAD, an LLVM context-local scope ID, or NIL for a non-atomic instruction."
  (let ((code (%instruction-atomic-sync-scope instruction)))
    (case code
      (0 nil)
      (1 :single-thread)
      (2 :system)
      (otherwise (cl:1- code)))))

(defun set-instruction-atomic-sync-scope (instruction scope)
  "Set the synchronization scope of an atomic instruction to :SYSTEM or :SINGLE-THREAD."
  (%set-instruction-atomic-sync-scope instruction (sync-scope-index 'set-instruction-atomic-sync-scope scope)))

(defun instruction-cmpxchg-success-ordering (instruction)
  "Return the success ordering of a cmpxchg, or NIL."
  (atomic-ordering-name (%instruction-cmpxchg-success-ordering instruction)))

(defun instruction-cmpxchg-failure-ordering (instruction)
  "Return the failure ordering of a cmpxchg, or NIL."
  (atomic-ordering-name (%instruction-cmpxchg-failure-ordering instruction)))

(defun set-instruction-cmpxchg-orderings (instruction success failure)
  "Validate and update the success and failure orderings of a cmpxchg."
  (%set-instruction-cmpxchg-orderings instruction
    (atomic-ordering-index 'set-instruction-cmpxchg-orderings success)
    (atomic-ordering-index 'set-instruction-cmpxchg-orderings failure)))

(defun instruction-atomic-rmw-operation (instruction)
  "Return the operation keyword of an atomicrmw, or NIL."
  (let ((index (%instruction-atomic-rmw-operation instruction)))
    (cl:when (plusp index)
      (nth (cl:1- index) *atomic-rmw-operations*))))

(defun set-instruction-atomic-rmw-operation (instruction operation)
  "Change the operation of an atomicrmw with the same value type."
  (%set-instruction-atomic-rmw-operation instruction
    (operation-index 'set-instruction-atomic-rmw-operation operation *atomic-rmw-operations*)))

(defun memory-size-operand (builder size)
  (cl:or (operand-value size)
         (progn
           (check-type size (unsigned-byte 64))
           (const :i64 size (builder-context builder)))))

(defun build-alloca (builder type &rest options)
  "Allocate TYPE on the stack. :COUNT accepts an LLVM integer or a nonnegative i64 literal."
  (destructuring-bind (&key (name "") count alignment) (instruction-name-options options)
    (%alloca builder (resolve-type type (builder-context builder))
             (cl:when count (memory-size-operand builder count)) (cl:or alignment 0) name)))

(defun build-load (builder type pointer &rest options)
  "Load TYPE from POINTER. :ORDERING makes the load atomic. :ALIGNMENT is in bytes."
  (destructuring-bind (&key (name "") alignment volatile ordering (sync-scope :system))
      (instruction-name-options options)
    (%load builder (resolve-type type (builder-context builder)) pointer (cl:or alignment 0) volatile
           (cl:if ordering (atomic-ordering-index 'build-load ordering) 0)
           (sync-scope-index 'build-load sync-scope) name)))

(defun build-store (builder value pointer &key alignment volatile ordering (sync-scope :system))
  "Store VALUE at POINTER. :ORDERING makes the store atomic. :ALIGNMENT is in bytes."
  (%store builder value pointer (cl:or alignment 0) volatile
          (cl:if ordering (atomic-ordering-index 'build-store ordering) 0)
          (sync-scope-index 'build-store sync-scope)))

(defun build-gep (builder source-type pointer indices &rest options)
  "Emit GEP using LLVM integer values or signed i32 index literals. Accept :IN-BOUNDS, :NUW, and :NUSW."
  (destructuring-bind (&key (name "") in-bounds nuw nusw) (instruction-name-options options)
    (let* ((ctx (builder-context builder))
           (type (resolve-type source-type ctx)))
      (call-with-handles (gep-indices indices ctx)
                         (lambda (data count) (%gep builder type pointer data count in-bounds nuw nusw name))))))

(defun build-gep-in-bounds (builder source-type pointer indices &rest options)
  "Emit an inbounds GEP. The caller establishes LLVM's inbounds preconditions."
  (apply #'build-gep builder source-type pointer indices :in-bounds t
         (instruction-name-options options)))

(defun build-fence (builder ordering &key (sync-scope :system))
  "Emit an atomic fence with ORDERING and optional :SYNC-SCOPE."
  (%fence builder (atomic-ordering-index 'build-fence ordering)
          (sync-scope-index 'build-fence sync-scope)))

(defun build-atomic-rmw (builder operation pointer value
                         &key (ordering :seq-cst) (sync-scope :system) alignment volatile (name ""))
  "Atomically update POINTER using OPERATION and VALUE, and return the previous value."
  (%atomic-rmw builder
               (operation-index 'build-atomic-rmw operation *atomic-rmw-operations*)
               pointer value (atomic-ordering-index 'build-atomic-rmw ordering)
               (sync-scope-index 'build-atomic-rmw sync-scope) (cl:or alignment 0) volatile name))

(defun build-cmpxchg (builder pointer expected replacement
                      &key (success-ordering :seq-cst) (failure-ordering :monotonic)
                      (sync-scope :system) alignment volatile weak (name ""))
  "Compare and exchange atomically. Return the LLVM pair of the old value and the i1 success flag."
  (%cmpxchg builder pointer expected replacement
            (atomic-ordering-index 'build-cmpxchg success-ordering)
            (atomic-ordering-index 'build-cmpxchg failure-ordering)
            (sync-scope-index 'build-cmpxchg sync-scope) (cl:or alignment 0) volatile weak name))

(defun build-memcpy (builder destination source length
                     &key destination-alignment source-alignment volatile)
  "Copy between equal or disjoint regions. LENGTH accepts an LLVM integer or an i64 literal."
  (%memcpy builder destination source (memory-size-operand builder length)
           (cl:or destination-alignment 0) (cl:or source-alignment 0) volatile))

(defun build-memmove (builder destination source length
                      &key destination-alignment source-alignment volatile)
  "Emit a byte copy between regions that may overlap."
  (%memmove builder destination source (memory-size-operand builder length)
            (cl:or destination-alignment 0) (cl:or source-alignment 0) volatile))

(defun build-memset (builder destination byte length &key destination-alignment volatile)
  "Fill LENGTH bytes starting at DESTINATION using an LLVM i8 value or a byte literal."
  (let ((value (cl:or (operand-value byte)
                      (progn
                        (check-type byte (unsigned-byte 8))
                        (const :i8 byte (builder-context builder))))))
    (%memset builder destination value (memory-size-operand builder length)
             (cl:or destination-alignment 0) volatile)))

(defun build-malloc (builder allocator type count &key (name ""))
  "Allocate COUNT elements via ALLOCATOR. The byte count must fit the allocator's integer parameter type."
  (let* ((operand (cl:or (operand-value count)
                         (progn
                           (check-type count (integer 0))
                           (ensure-value count (type-subtype (function-signature allocator) 1)))))
         (type (resolve-type type (builder-context builder))))
    (%malloc builder allocator type operand name)))

(defun build-free (builder deallocator pointer)
  "Call DEALLOCATOR on POINTER and return the call instruction."
  (%free builder deallocator pointer))

(defun build-ptr-diff (builder type left right &key (name ""))
  "Return the signed element distance between LEFT and RIGHT in one allocation. Use the layout's pointer index width."
  (%ptr-diff builder (resolve-type type (builder-context builder)) left right name))

(defun add-global-string (contents &key (name "") (nul-terminated t) (module (ensure-module)))
  "Add a private constant byte array from UTF-8 text or octets and return its global handle."
  (call-with-octets contents
    (lambda (data length) (%add-global-string module name data length nul-terminated))))

(defun alloca (type-spec &rest options)
  "Allocate TYPE-SPEC. :INIT or positional INIT initializes its first element and requires a positive count."
  (destructuring-bind (&key (init nil init-p) (name "") count alignment)
      (cl:if (cl:and options (cl:not (member (first options) '(:init :name :count :alignment))))
        (list* :init (first options) (instruction-name-options (rest options)))
        options)
    (let* ((builder (ensure-builder))
           (type (resolve-type type-spec (builder-context builder)))
           (initial (cl:when init-p (coerce-operand init type 'alloca)))
           (slot (build-alloca builder type :name name :count count :alignment alignment)))
      (cl:when init-p
        (build-store builder initial slot :alignment alignment))
      slot)))

(defun load (type-spec pointer &rest options)
  (apply #'build-load (ensure-builder) type-spec (ensure-value pointer :ptr) options))

(defun store (value pointer &rest options)
  (destructuring-bind (&key type alignment volatile ordering (sync-scope :system))
      (cl:if (cl:and options (cl:not (member (first options)
                                             '(:type :alignment :volatile :ordering :sync-scope))))
        (list* :type (first options) (rest options))
        options)
    (let* ((builder (ensure-builder))
           (type (cl:or type (cl:and (global-p pointer) (global-storage-type pointer))))
           (val (cl:if type
                  (coerce-operand value (resolve-type type (builder-context builder)) 'store)
                  (ensure-value value))))
      (build-store builder val (ensure-value pointer :ptr) :alignment alignment :volatile volatile
                   :ordering ordering :sync-scope sync-scope)
      (values))))

(defun gep (source-type pointer indices &rest options)
  (apply #'build-gep (ensure-builder) source-type (ensure-value pointer :ptr) indices options))

(defun gep-in-bounds (source-type pointer indices &rest options)
  (apply #'build-gep-in-bounds (ensure-builder) source-type (ensure-value pointer :ptr) indices options))

(defun fence (ordering &key (sync-scope :system))
  (build-fence (ensure-builder) ordering :sync-scope sync-scope))

(defun atomic-rmw (operation pointer value &key type (ordering :seq-cst) (sync-scope :system)
                   alignment volatile (name ""))
  "Atomically update POINTER and return its old value. :TYPE supplies the type of a Lisp literal."
  (let* ((builder (ensure-builder))
         (type (cl:or type (cl:and (global-p pointer) (global-storage-type pointer))))
         (operand (cl:if type
                    (coerce-operand value (resolve-type type (builder-context builder)) 'atomic-rmw)
                    (ensure-value value))))
    (build-atomic-rmw builder operation (ensure-value pointer :ptr) operand :ordering ordering
                      :sync-scope sync-scope :alignment alignment :volatile volatile :name name)))

(defun cmpxchg (pointer expected replacement
                &key type (success-ordering :seq-cst) (failure-ordering :monotonic)
                (sync-scope :system) alignment volatile weak (name ""))
  "Compare and exchange atomically. Infer literal types from a typed operand or :TYPE."
  (let* ((builder (ensure-builder))
         (type (cl:or type (cl:and (global-p pointer) (global-storage-type pointer))))
         (type (cl:if type (resolve-type type (builder-context builder))
                 (infer-literal-type (list expected replacement))))
         (old (ensure-value expected type))
         (new (ensure-value replacement type)))
    (build-cmpxchg builder (ensure-value pointer :ptr) old new :success-ordering success-ordering
                   :failure-ordering failure-ordering :sync-scope sync-scope :alignment alignment
                   :volatile volatile :weak weak :name name)))

(defun memcpy (destination source length &rest options)
  (apply #'build-memcpy (ensure-builder) (ensure-value destination :ptr) (ensure-value source :ptr)
         length options))

(defun memmove (destination source length &rest options)
  (apply #'build-memmove (ensure-builder) (ensure-value destination :ptr) (ensure-value source :ptr)
         length options))

(defun memset (destination byte length &rest options)
  (apply #'build-memset (ensure-builder) (ensure-value destination :ptr) byte length options))

(defun malloc (allocator type count &key (name ""))
  "Allocate COUNT elements via an explicit allocator function or name in the current module."
  (build-malloc (ensure-builder) (resolve-callee allocator) type count :name name))

(defun free (deallocator pointer)
  "Release POINTER via an explicit deallocator function or name in the current module."
  (build-free (ensure-builder) (resolve-callee deallocator) (ensure-value pointer :ptr)))

(defun ptr-diff (type left right &key (name ""))
  "Subtract two addresses in units of TYPE, using the current module's data layout."
  (build-ptr-diff (ensure-builder) type (ensure-value left :ptr) (ensure-value right :ptr) :name name))
