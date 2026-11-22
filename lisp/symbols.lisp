(in-package #:zk-llvm)

(define-operation global-declaration-p "global_is_declaration" :bool (global :handle))
(define-operation function-declaration-p "function_is_declaration" :bool (function :handle))
(define-operation global-address-space "global_address_space" :uint64 (global :handle))
(define-operation set-global-constant "set_global_constant" :effect
    (global :handle) (constant :bool))
(define-operation clear-global-initializer "clear_global_initializer" :effect (global :handle))
(define-operation global-externally-initialized-p "global_is_externally_initialized" :bool
    (global :handle))
(define-operation set-global-externally-initialized "set_global_externally_initialized" :effect
    (global :handle) (externally-initialized :bool))
(define-operation (%global-thread-local-mode global-thread-local-mode) "global_thread_local_mode" :uint64
    (global :handle))
(define-operation (%set-global-thread-local-mode set-global-thread-local-mode)
    "set_global_thread_local_mode" :effect (global :handle) (mode :uint32))

(define-operation (%global-visibility global-visibility) "global_visibility" :uint64
    (global :handle))
(define-operation (%set-global-visibility set-global-visibility) "set_global_visibility" :effect
    (global :handle) (visibility :uint32))
(define-operation (%function-visibility function-visibility) "function_visibility" :uint64
    (function :handle))
(define-operation (%set-function-visibility set-function-visibility) "set_function_visibility" :effect
    (function :handle) (visibility :uint32))

(define-operation (%global-dll-storage-class global-dll-storage-class) "global_dll_storage_class" :uint64
    (global :handle))
(define-operation (%set-global-dll-storage-class set-global-dll-storage-class)
    "set_global_dll_storage_class" :effect (global :handle) (storage-class :uint32))
(define-operation (%function-dll-storage-class function-dll-storage-class)
    "function_dll_storage_class" :uint64 (function :handle))
(define-operation (%set-function-dll-storage-class set-function-dll-storage-class)
    "set_function_dll_storage_class" :effect (function :handle) (storage-class :uint32))

(define-operation global-section "global_section" :optional-text (global :handle))
(define-operation function-section "function_section" :optional-text (function :handle))
(define-operation (%set-global-section set-global-section) "set_global_section" :effect
    (global :handle) (name :cstring) (present :bool))
(define-operation (%set-function-section set-function-section) "set_function_section" :effect
    (function :handle) (name :cstring) (present :bool))

(define-operation get-or-insert-comdat "get_or_insert_comdat" :comdat
    (module :handle) (name :cstring))
(define-operation global-comdat "global_comdat" :comdat (global :handle))
(define-operation function-comdat "function_comdat" :comdat (function :handle))
(define-operation set-global-comdat "set_global_comdat" :effect
    (global :handle) (comdat :optional-handle))
(define-operation set-function-comdat "set_function_comdat" :effect
    (function :handle) (comdat :optional-handle))
(define-operation (%comdat-selection-kind comdat-selection-kind) "comdat_selection_kind" :uint64
    (comdat :handle))
(define-operation (%set-comdat-selection-kind set-comdat-selection-kind)
    "set_comdat_selection_kind" :effect (comdat :handle) (kind :uint32))

(defparameter *visibility-modes* '(:default :hidden :protected))
(defparameter *dll-storage-classes* '(:default :import :export))
(defparameter *thread-local-modes* '(nil :general-dynamic :local-dynamic :initial-exec :local-exec))
(defparameter *comdat-selection-kinds* '(:any :exact-match :largest :no-duplicates :same-size))

(defun global-visibility (global)
  (nth (%global-visibility global) *visibility-modes*))

(defun set-global-visibility (global visibility)
  (%set-global-visibility global
    (operation-index 'set-global-visibility visibility *visibility-modes*)))

(defun function-visibility (function)
  (nth (%function-visibility function) *visibility-modes*))

(defun set-function-visibility (function visibility)
  (%set-function-visibility function
    (operation-index 'set-function-visibility visibility *visibility-modes*)))

(defun global-dll-storage-class (global)
  (nth (%global-dll-storage-class global) *dll-storage-classes*))

(defun set-global-dll-storage-class (global storage-class)
  (%set-global-dll-storage-class global
    (operation-index 'set-global-dll-storage-class storage-class *dll-storage-classes*)))

(defun function-dll-storage-class (function)
  (nth (%function-dll-storage-class function) *dll-storage-classes*))

(defun set-function-dll-storage-class (function storage-class)
  (%set-function-dll-storage-class function
    (operation-index 'set-function-dll-storage-class storage-class *dll-storage-classes*)))

(defun global-thread-local-mode (global)
  (nth (%global-thread-local-mode global) *thread-local-modes*))

(defun set-global-thread-local-mode (global mode)
  (%set-global-thread-local-mode global
    (operation-index 'set-global-thread-local-mode mode *thread-local-modes*)))

(defun set-global-section (global section)
  "Set GLOBAL's target section name, or clear it when SECTION is NIL."
  (%set-global-section global (cl:or section "") section))

(defun set-function-section (function section)
  "Set FUNCTION's target section name, or clear it when SECTION is NIL."
  (%set-function-section function (cl:or section "") section))

(defun comdat-selection-kind (comdat)
  (nth (%comdat-selection-kind comdat) *comdat-selection-kinds*))

(defun set-comdat-selection-kind (comdat kind)
  (%set-comdat-selection-kind comdat
    (operation-index 'set-comdat-selection-kind kind *comdat-selection-kinds*)))
