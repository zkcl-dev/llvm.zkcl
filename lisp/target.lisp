(in-package #:zk-llvm)

(define-operation host-triple "host_triple" :text)
(define-operation host-cpu "host_cpu" :text)
(define-operation host-features "host_features" :text)
(define-operation (%make-target-machine make-target-machine) "make_target_machine_with_options" :target-machine
    (triple :cstring) (optimization :uint32) (cpu :cstring) (features :cstring)
    (relocation :uint32) (code-model :uint32) (abi :cstring))
(define-operation target-triple "target_triple" :text (machine :handle))
(define-operation target-cpu "target_cpu" :text (machine :handle))
(define-operation target-features "target_features" :text (machine :handle))
(define-operation target-abi "target_abi" :optional-text (machine :handle))
(define-operation target-backend-name "target_backend_name" :text (machine :handle))
(define-operation target-backend-description "target_backend_description" :text (machine :handle))
(define-operation target-data-layout "target_data_layout" :text (machine :handle))
(define-operation module-target-triple "module_target_triple" :optional-text (module :handle))
(define-operation module-data-layout "module_data_layout" :optional-text (module :handle))
(define-operation (%set-module-target-triple set-module-target-triple) "set_module_target_triple" :effect
    (module :handle) (triple :cstring))
(define-operation (%make-data-layout make-data-layout) "make_data_layout" :data-layout (text :cstring))
(define-operation target-layout "target_layout" :data-layout (machine :handle))
(define-operation module-layout "module_layout" :data-layout (module :handle))
(define-operation (%set-module-layout set-module-layout) "set_module_layout" :effect
    (module :handle) (layout :handle))
(define-operation layout-text "layout_text" :text (layout :handle))
(define-operation layout-little-endian-p "layout_little_endian_p" :bool (layout :handle))
(define-operation (%layout-pointer-size-bits layout-pointer-size-bits) "layout_pointer_size_bits" :uint64
    (layout :handle) (address-space :uint32))
(define-operation (%layout-pointer-index-size-bits layout-pointer-index-size-bits) "layout_pointer_index_size_bits" :uint64
    (layout :handle) (address-space :uint32))
(define-operation layout-non-integral-pointer-p "layout_non_integral_pointer_p" :bool
    (layout :handle) (address-space :uint32))
(define-operation (%layout-size-in-bits layout-size-in-bits) "layout_size_in_bits" :uint64
    (layout :handle) (type :handle))
(define-operation (%layout-store-size layout-store-size) "layout_store_size" :uint64
    (layout :handle) (type :handle))
(define-operation (%layout-alloc-size layout-alloc-size) "layout_alloc_size" :uint64
    (layout :handle) (type :handle))
(define-operation (%layout-abi-alignment layout-abi-alignment) "layout_abi_alignment" :uint64
    (layout :handle) (type :handle))
(define-operation (%layout-preferred-alignment layout-preferred-alignment) "layout_preferred_alignment" :uint64
    (layout :handle) (type :handle))
(define-operation (%layout-field-offset layout-field-offset) "layout_field_offset" :uint64
    (layout :handle) (struct :handle) (index :uint32))
(define-operation configure-module-target "configure_module_target" :effect
    (module :handle) (machine :handle))
(define-operation (%target-abi-size target-abi-size) "target_abi_size" :uint64
    (machine :handle) (type :handle))
(define-operation (%target-abi-alignment target-abi-alignment) "target_abi_alignment_bytes" :uint64
    (machine :handle) (type :handle))
(define-operation (%target-field-offset target-field-offset) "target_field_offset" :uint64
    (machine :handle) (struct :handle) (index :uint32))
(define-operation (%optimize-module optimize-module) "optimize_module" :module
    (module :handle) (pipeline :cstring) (machine :optional-handle))
(define-operation (%optimize-module-with-options optimize-module) "optimize_module_with_options" :module
    (module :handle) (pipeline :cstring) (machine :optional-handle)
    (flags :uint32) (aa-pipeline :cstring) (opt-cap :int64) (promotion-cap :int64) (threshold :int32))
(define-operation (%optimize-function optimize-function) "optimize_function" :module
    (function :handle) (pipeline :cstring) (machine :optional-handle)
    (flags :uint32) (aa-pipeline :cstring) (opt-cap :int64) (promotion-cap :int64) (threshold :int32))
(define-operation (%emit-object emit-object) "emit_object" :bytes
    (module :handle) (machine :optional-handle))
(define-operation (%emit-assembly emit-assembly) "emit_assembly" :text
    (module :handle) (machine :optional-handle))

(defun make-target-machine (&key (triple (host-triple)) (optimization :default)
                                (cpu "") (features "") (relocation :pic)
                                (code-model :default) (abi ""))
  "Select a code generation target. FEATURES is a comma-separated string of +NAME and -NAME options.
Empty CPU, FEATURES and ABI strings select the LLVM defaults for TRIPLE."
  (dolist (text (list triple cpu features abi))
    (cl:when (find (code-char 0) (the string text))
      (error 'llvm-error :operation 'make-target-machine
             :message "LLVM target options cannot contain embedded NUL characters.")))
  (%make-target-machine triple (operation-index 'make-target-machine optimization
                                               '(:none :less :default :aggressive))
                        cpu features
                        (operation-index 'make-target-machine relocation
                                         '(:default :static :pic :dynamic-no-pic :ropi :rwpi :ropi-rwpi))
                        (operation-index 'make-target-machine code-model
                                         '(:default :tiny :small :kernel :medium :large :jit-default))
                        abi))

(defun set-module-target-triple (module triple)
  "Set the module's normalized target triple before lowering target-dependent IR.
An empty string clears the triple."
  (cl:when (find (code-char 0) (the string triple))
    (error 'llvm-error :operation 'set-module-target-triple
           :message "LLVM target triples cannot contain embedded NUL characters."))
  (%set-module-target-triple module triple))

(defun make-data-layout (text)
  "Parse LLVM's target data-layout string. An empty string selects LLVM's default layout."
  (cl:when (find (code-char 0) (the string text))
    (error 'llvm-error :operation 'make-data-layout
           :message "LLVM data layouts cannot contain embedded NUL characters."))
  (%make-data-layout text))

(defun set-module-layout (module layout)
  "Set the layout before lowering target-dependent IR. Accept a layout handle or string.
An empty layout clears the explicit layout."
  (etypecase layout
    (data-layout (%set-module-layout module layout))
    (string (let ((parsed (make-data-layout layout)))
              (unwind-protect (%set-module-layout module parsed)
                (release parsed))))))

(defun layout-pointer-size-bits (layout &optional (address-space 0))
  (%layout-pointer-size-bits layout address-space))

(defun layout-pointer-index-size-bits (layout &optional (address-space 0))
  "Return the bit width of GEP indices in ADDRESS-SPACE."
  (%layout-pointer-index-size-bits layout address-space))

(defun layout-size-in-bits (layout type)
  (%layout-size-in-bits layout (resolve-type type)))

(defun layout-store-size (layout type)
  "Return the number of bytes LLVM may overwrite when storing a value of TYPE."
  (%layout-store-size layout (resolve-type type)))

(defun layout-alloc-size (layout type)
  "Return the byte stride for TYPE when laid out in an array."
  (%layout-alloc-size layout (resolve-type type)))

(defun layout-abi-alignment (layout type)
  (%layout-abi-alignment layout (resolve-type type)))

(defun layout-preferred-alignment (layout type)
  (%layout-preferred-alignment layout (resolve-type type)))

(defun layout-field-offset (layout struct index)
  (%layout-field-offset layout (resolve-type struct) index))

(defun target-abi-size (machine type)
  (%target-abi-size machine (resolve-type type)))

(defun target-abi-alignment (machine type)
  (%target-abi-alignment machine (resolve-type type)))

(defun target-field-offset (machine struct index)
  (%target-field-offset machine (resolve-type struct) index))

(defun normalize-pass-pipeline (pipeline)
  (etypecase pipeline
    (string pipeline)
    (keyword (ecase pipeline
               (:o0 "default<O0>")
               (:o1 "default<O1>")
               ((:o2 :default) "default<O2>")
               (:o3 "default<O3>")
               (:os "default<Os>")
               (:oz "default<Oz>")))))

(defstruct (pass-options (:copier nil))
  "Tuning for LLVM's new pass manager.
Use :DEFAULT to retain LLVM's defaults for optional flags, T to enable, or NIL to disable.
AA-PIPELINE accepts NIL for LLVM's default or an empty string to select no analyses.
LLVM's default<O*> and LTO aliases choose vectorization according to their optimization level.
Passes named explicitly can take parameters in the pipeline text."
  (verify-each nil :type boolean)
  (debug-logging nil :type boolean)
  (aa-pipeline nil :type (cl:or null string))
  (loop-interleaving :default :type (member :default nil t))
  (loop-vectorization :default :type (member :default nil t))
  (slp-vectorization :default :type (member :default nil t))
  (loop-unrolling :default :type (member :default nil t))
  (forget-all-scev-in-loop-unroll :default :type (member :default nil t))
  (call-graph-profile :default :type (member :default nil t))
  (merge-functions :default :type (member :default nil t))
  (licm-mssa-opt-cap nil :type (cl:or null (unsigned-byte 32)))
  (licm-mssa-no-acc-for-promotion-cap nil :type (cl:or null (unsigned-byte 32)))
  (inliner-threshold nil :type (cl:or null (signed-byte 32))))

(defun check-pass-text (text operation)
  (cl:when (find (code-char 0) (the string text))
    (error 'llvm-error :operation operation
           :message "LLVM pass pipelines cannot contain embedded NUL characters."))
  text)

(defun pass-option-arguments (options operation)
  (cl:unless options
    (return-from pass-option-arguments '(0 "" -1 -1 0)))
  (check-type options pass-options)
  (flet ((toggle (flag shift)
           (ash (ecase flag (:default 0) ((nil) 1) ((t) 2)) shift)))
    (let ((aa-pipeline (pass-options-aa-pipeline options)))
      (list (logior (cl:if (pass-options-verify-each options) 1 0)
                    (cl:if (pass-options-debug-logging options) 2 0)
                    (cl:if aa-pipeline 4 0)
                    (toggle (pass-options-loop-interleaving options) 3)
                    (toggle (pass-options-loop-vectorization options) 5)
                    (toggle (pass-options-slp-vectorization options) 7)
                    (toggle (pass-options-loop-unrolling options) 9)
                    (toggle (pass-options-forget-all-scev-in-loop-unroll options) 11)
                    (toggle (pass-options-call-graph-profile options) 13)
                    (toggle (pass-options-merge-functions options) 15)
                    (cl:if (pass-options-inliner-threshold options) (ash 1 17) 0))
            (cl:if aa-pipeline (check-pass-text aa-pipeline operation) "")
            (cl:or (pass-options-licm-mssa-opt-cap options) -1)
            (cl:or (pass-options-licm-mssa-no-acc-for-promotion-cap options) -1)
            (cl:or (pass-options-inliner-threshold options) 0)))))

(defun optimize-module (module &rest arguments)
  "Optimize a verified copy in a fresh LLVM context. The optional positional pipeline
defaults to :O2. It accepts LLVM pipeline strings and optimization-level keywords.
Pass :OPTIONS from MAKE-PASS-OPTIONS and :TARGET-MACHINE as keyword arguments."
  (let ((pipeline (cl:if (oddp (length arguments)) (pop arguments) :o2)))
    (destructuring-bind (&key target-machine options) arguments
      (let ((pipeline (check-pass-text (normalize-pass-pipeline pipeline) 'optimize-module)))
        (cl:if options
               (apply #'%optimize-module-with-options module pipeline target-machine
                      (pass-option-arguments options 'optimize-module))
               (%optimize-module module pipeline target-machine))))))

(defun optimize-function (function pipeline &key target-machine options)
  "Run a function pipeline on FUNCTION in a fresh copy of its module and return the verified copy.
PIPELINE is a string such as \"instcombine,simplifycfg\" or \"function-simplification<O2>\".
OPTIONS comes from MAKE-PASS-OPTIONS. Original IR handles remain valid."
  (apply #'%optimize-function function (check-pass-text pipeline 'optimize-function) target-machine
         (pass-option-arguments options 'optimize-function)))

(defun emit-object (module &optional target-machine)
  "Return the compiled object file as an octet vector."
  (%emit-object module target-machine))

(defun emit-assembly (module &optional target-machine)
  "Return the compiled assembly as a string."
  (%emit-assembly module target-machine))

(defmethod print-object ((object target-machine) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (cl:if (handle-alive-p object) (target-triple object) ":RELEASED") stream)))

(defmethod print-object ((object data-layout) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (cl:if (handle-alive-p object) (layout-text object) ":RELEASED") stream)))
